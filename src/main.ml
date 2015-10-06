(*
 * skkserv-lite
 *      SKK server using sqlite3 dictionaries
 *)

open Ext

open Unix

let (>>=) = Lwt.(>>=)

exception No_addr_info
exception Invalid_user

type mode =
  | CREATE
  | FILTER
  | SERVER_DAEMON
  | SERVER_FOREGROUND
  | VERSION


let mode = ref FILTER

external _exit : int -> unit = "unix_exit"

(* Create mode options *)
let out_file = ref "SKK-JISYO.sqlite"

(* Server mode options *)
let host = ref ""
let port = ref "1178"
let address_family = ref None
let user = ref ""

(* Daemon mode options *)
let pid_file = ref None

let rev_argv = ref []

let dicts = ref []

let usage =
  "usage:\n" ^
  "  skkserv-lite SKK-JISYO.sqlite [...]\n" ^
  "  skkserv-lite -C [-o SKK-JISYO.sqlite] SKK-JISYO [SKK-JISYO ...]\n" ^
  "  skkserv-lite -d [options] SKK-JISYO.sqlite [...]\n" ^
  "  skkserv-lite -f [options] SKK-JISYO.sqlite [...]\n"

let rec speclist = [
  "-help",      Arg.Unit help, "";
  "--help",     Arg.Unit help, "";
  "-h",         Arg.Unit help,
  " show help message";

  "-4",         Arg.Unit (fun () -> address_family := Some PF_INET),
  " listen on IPv4 only";

  "-6",         Arg.Unit (fun () -> address_family := Some PF_INET6),
  " listen on IPv6 only";

  "-C",         Arg.Unit (fun () -> mode := CREATE),
  " create SKK-JISYO.sqlite from SKK-JISYO";

  "-P",         Arg.String (fun s -> pid_file := Some s),
  " write the process id to PIDFILE";

  "-b",         Arg.Set_string host,
  "HOST bind to HOST";

  "-d",         Arg.Unit (fun () -> mode := SERVER_DAEMON),
  " run server as daemon";

  "-f",         Arg.Unit (fun () -> mode := SERVER_FOREGROUND),
  " run server in the foreground";

  "-o",         Arg.Set_string out_file,
  " sqlite3 dictionary to create (default is SKK-JISYO.sqlite)";

  "-p",         Arg.Set_string port,
  " port to listen on (default is 1178)";

  "-u",         Arg.Set_string user,
  "USER setuid to USER on daemon mode";

  "--version",  Arg.Unit (fun () -> mode := VERSION), "";
  "-v",         Arg.Unit (fun () -> mode := VERSION),
  " show version";
]
and help () = raise (Arg.Help (Arg.usage_string (Arg.align speclist) usage))


let check_argv () =
  if !rev_argv = [] then begin
    prerr_endline "No dictionary specified";
    exit 1
  end
;;

let open_dictionaries () =
  dicts := List.rev_map Dict.opendict !rev_argv
;;

let create_pid_file file =
  let pid = Printf.sprintf "%d\n" (getpid ()) in
  let fd = openfile file [O_RDWR; O_CREAT] 0o644 in
  lockf fd F_TLOCK 0;
  set_close_on_exec fd;
  let len = write fd pid 0 (String.length pid) in
  ftruncate fd len;
  at_exit (fun () -> try Unix.unlink file with _ -> ())
;;

let daemonize () =
  let fork' () =
    match fork () with
    | (-1) -> exit (-1)
    | 0 -> ()
    | _ -> _exit 0
  in

  fork' ();

  if setsid () = -1 then
    exit (-1);

  fork' ();

  chdir "/";

  let fd = openfile "/dev/null" [O_RDWR] 0o644 in
  dup2 fd stdin;
  dup2 fd stdout;
  dup2 fd stderr;
  close fd
;;

let rec write_all fd buf ofs len =
  Lwt_unix.write fd buf ofs len >>= fun n ->
  if n = 0 || n = len then
    Lwt.return ()
  else
    write_all fd buf (ofs + n) (len - n)
;;

let rec serve session ifd ofd rbuf () =
  let rec loop req =
    match Skkserv.serve session req with
    | None ->
        Lwt.return None
    | Some ("", rest) ->
        Lwt.return (Some rest)
    | Some (resp, "") ->
        write_all ofd resp 0 (String.length resp) >>= fun () ->
        Lwt.return (Some "")
    | Some (resp, rest) ->
        write_all ofd resp 0 (String.length resp) >>= fun () ->
        loop rest
  in

  let buf = String.create 4096 in
  Lwt_unix.read ifd buf 0 4096 >>= function
  | 0 -> Lwt.return ()
  | n ->
      loop @@ rbuf ^ (String.sub buf 0 n) >>= function
      | None -> Lwt.return ()
      | Some rest -> serve session ifd ofd rest ()
;;

let start_session sock =
  let session = Skkserv.create ~fd:(Lwt_unix.unix_file_descr sock) dicts in
  let finalize () =
    Lwt_unix.close sock
  in
  Lwt.on_failure
    (serve session sock sock "" () >>= finalize)
    (fun e -> Lwt_log.error (Printexc.to_string e); ignore @@ finalize ());
;;

let accept_connection conn =
  let sock, _ = conn in
  start_session sock;
  Lwt_log.info "New connection" >>= Lwt.return
;;

let run1 sock =
  let rec loop () =
    Lwt_unix.accept sock >>= accept_connection >>= loop
  in
  loop ()
;;

let create_socket ?(v6only=false) ai =
  let open Lwt_unix in
  let s = socket ai.ai_family ai.ai_socktype ai.ai_protocol in
  setsockopt s SO_REUSEADDR true;
  if ai.ai_family = PF_INET6 then
    setsockopt s IPV6_ONLY v6only;
  bind s ai.ai_addr;
  listen s 5;
  s
;;

let create_sockets host port opt_family =
  let flags = [AI_SOCKTYPE SOCK_STREAM; AI_PASSIVE] in
  match host, opt_family with
  | "", None ->
      getaddrinfo host port ((AI_FAMILY PF_INET6) :: flags)
      |> ListLabels.map ~f:(create_socket ~v6only:false)
  | "", Some family ->
      getaddrinfo host port ((AI_FAMILY family) :: flags)
      |> ListLabels.map ~f:(create_socket ~v6only:true)
  | _, None ->
      getaddrinfo host port flags
      |> ListLabels.map ~f:(create_socket ~v6only:true)
  | _, Some family ->
      getaddrinfo host port ((AI_FAMILY family) :: flags)
      |> ListLabels.map ~f:(create_socket ~v6only:true)
;;

let server () =
  check_argv ();

  try
    if !mode = SERVER_DAEMON then begin
      rev_argv := List.map (fun a ->
          if Filename.is_relative a then
            Filename.concat (Sys.getcwd ()) a
          else
            a
        ) !rev_argv;
      handle_unix_error daemonize ();
      match !pid_file with
      | None -> ()
      | Some file -> handle_unix_error create_pid_file file
    end;

    if !user <> "" then begin
      let uid = try int_of_string !user with
        | Failure _ ->
            try let pwent = getpwnam !user in pwent.pw_uid with
            | Not_found -> raise Invalid_user
      in
      handle_unix_error setuid uid
    end;

    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    Sys.set_signal Sys.sighup
      (Sys.Signal_handle (fun _ -> open_dictionaries ()));

    open_dictionaries ();

    let listen_socks = create_sockets !host !port !address_family in
    if listen_socks = [] then
      raise No_addr_info;

    Lwt_main.run @@ Lwt.join @@ ListLabels.map listen_socks ~f:run1
  with
  | Invalid_user ->
      Printf.fprintf Pervasives.stderr "Invalid user: %s\n" !user;
      exit 1
  | No_addr_info ->
      prerr_endline "No address to bind";
      exit 1
;;

let filter () =
  let open Lwt_unix in
  check_argv ();
  open_dictionaries ();
  let skkserv = Skkserv.create ~fd:Unix.stdin dicts in
  Lwt_main.run @@ serve skkserv stdin stdout "" ()
;;

let create () =
  check_argv ();

  let dict = Dict.opendict !out_file in
  let stream_of_jisyo jisyo =
    let ic = open_in jisyo in
    let okuri_ari = ref true in
    let queue = ref [] in
    let rec loop = function
      | x :: xs ->
          queue := xs;
          Some x
      | [] ->
          match input_line ic with
          | "" ->
              loop []
          | ";; okuri-ari entries." ->
              okuri_ari := true;
              loop []
          | ";; okuri-nasi entries." ->
              okuri_ari := false;
              loop []
          | line when line.[0] = ';' ->
              loop []
          | line ->
              try
                let key, ent = String.split2 ' ' (Encode.utf8_of_eucjp line) in
                let subents = Str.split (Str.regexp "/") ent in
                let xs = List.map (fun e ->
                    let cand, anno = String.split2 ';' e in
                    (key, cand, anno, !okuri_ari)
                  ) subents
                in
                loop xs
              with _ -> loop []
    in
    Stream.from (fun _ -> try loop !queue with End_of_file -> None)
  in
  Dict.create_table dict;
  List.iter (fun jisyo -> Dict.add_from_stream dict (stream_of_jisyo jisyo))
            (List.rev !rev_argv)
;;

let () =
  Arg.parse (Arg.align speclist)
            (fun arg -> rev_argv := arg :: !rev_argv)
            usage;

  match !mode with
  | CREATE ->
      create ()
  | FILTER ->
      filter ()
  | SERVER_DAEMON | SERVER_FOREGROUND ->
      handle_unix_error server ()
  | VERSION ->
      print_endline Version.version
;;
