(*
 * skkserv-lite
 *      SKK server using sqlite3 dictionaries
 *)

open Ext
open Server_nb

open Unix

exception No_addr_info
exception Invalid_user

type mode =
  | CREATE
  | FILTER
  | SERVER_DAEMON
  | SERVER_FOREGROUND
  | VERSION


let mode = ref FILTER

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

let usage = "usage:
  skkserv-lite SKK-JISYO.sqlite [...]
  skkserv-lite -C [-o SKK-JISYO.sqlite] SKK-JISYO [SKK-JISYO ...]
  skkserv-lite -d [options] SKK-JISYO.sqlite [...]
  skkserv-lite -f [options] SKK-JISYO.sqlite [...]
"

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
    | _ -> exit 0
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

let prepare_socket ai =
  let s = socket ai.ai_family ai.ai_socktype ai.ai_protocol in
  set_nonblock s;
  setsockopt s SO_REUSEADDR true;
  bind s ai.ai_addr;
  listen s 5;
  s
;;

let server () =
  check_argv ();

  try
    let ai_options = match !address_family with
      | None -> [AI_SOCKTYPE SOCK_STREAM; AI_PASSIVE]
      | Some dom -> [AI_FAMILY dom; AI_SOCKTYPE SOCK_STREAM; AI_PASSIVE]
    in
    let addr_info_list = getaddrinfo !host !port ai_options in

    if addr_info_list = [] then
      raise No_addr_info;

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
        | Failure e ->
            try let pwent = getpwnam !user in pwent.pw_uid with
            | Not_found -> raise Invalid_user
      in
      handle_unix_error setuid uid
    end;

    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    Sys.set_signal Sys.sighup
      (Sys.Signal_handle (fun _ -> open_dictionaries ()));

    open_dictionaries ();

    let servers = Hashtbl.create 64 in

    let listen_socks = List.map prepare_socket addr_info_list in
    let writing_socks = ref [] in

    while true do
      let reading_socks = Hashtbl.fold
        (fun s _ accu -> if List.mem s !writing_socks then accu else s :: accu)
        servers listen_socks
      in
      let rsocks, wsocks, _ =
        try select reading_socks !writing_socks [] (-1.0)
        with Unix_error (EINTR, _, _) -> [], [], []
      in
      List.iter (fun s ->
          if List.mem s listen_socks then begin
            let nsock, _ = accept s in
            set_nonblock nsock;
            let skkserv = Skkserv.create ~socket:nsock dicts in
            let server = Server_nb.create (Skkserv.serve skkserv) in
            Hashtbl.add servers nsock server
          end else
            let server = Hashtbl.find servers s in
            match Server_nb.serve server s s with
            | Close ->
                close s;
                writing_socks := List.filter ((<>) s) !writing_socks;
                Hashtbl.remove servers s
            | Wait_readable ->
                writing_socks := List.filter ((<>) s) !writing_socks;
            | Wait_writable ->
                if not (List.mem s !writing_socks) then
                  writing_socks := s :: !writing_socks;
        ) (rsocks @ wsocks);
    done

  with
  | Invalid_user ->
      Printf.fprintf Pervasives.stderr "Invalid user: %s\n" !user;
      exit 1
  | No_addr_info ->
      prerr_endline "No address to bind";
      exit 1
;;

let filter () =
  check_argv ();
  open_dictionaries ();
  let skkserv = Skkserv.create ~socket:stdin dicts in
  let server = Server_nb.create (Skkserv.serve skkserv) in
  while true do
    match Server_nb.serve server ~in_fd:stdin ~out_fd:stdout with
    | Close -> exit 0
    | Wait_readable | Wait_writable -> ()
  done
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
