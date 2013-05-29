(*
 * skkserv.ml
 *      SKK server services
 *)

open Ext

open Unix

exception End
exception Error
exception Exit
exception Not_enough_data

type t = {
  in_fd : file_descr;
  out_fd : file_descr;
  mutable rbuf : string;
  mutable wbuf : string;
}

type command =
  | Close
  | Lookup of string
  | Get_version
  | Get_address
  | Complete of string
  | Unknown


let string_of_subentry = function
  | cand, [] -> cand
  | cand, annos -> String.concat ";" [cand; String.concat "," annos]
;;

let write_nb fd buf off len =
  try write fd buf off len
  with Unix_error (EWOULDBLOCK, _, _) | Unix_error (EAGAIN, _, _) -> 0
;;

let write_and_get_rest fd buf off len =
  let len' = write_nb fd buf off len in
  String.sub buf len' (len - len')

let create ~in_fd ~out_fd =
  { in_fd = in_fd; out_fd = out_fd; rbuf = ""; wbuf = "" }

(*
 * There are two types of commands.
 * One type has no arguments and another has one argument.
 * An argument starts just after command character and terminated by spaces.
 *)
let get_command req =
  if req = "" then
    raise Not_enough_data;

  let cmd = req.[0] in
  let rest = Str.string_after req 1 in

  let cmd1 f s =
    match Str.bounded_split_delim (Str.regexp "[ \t\r\n]+") s 2 with
    | [arg; rest] -> f arg, rest
    | _ -> raise Not_enough_data
  in

  match cmd with
  | '0' -> Close, rest
  | '1' -> cmd1 (fun x -> Lookup x) rest
  | '2' -> Get_version, rest
  | '3' -> Get_address, rest
  | '4' -> cmd1 (fun x -> Complete x) rest
  | _ -> Unknown, rest
;;

let do_lookup_cmd dicts arg f1 f2 =
  match Encode.utf8_of_eucjp arg with
  | "" -> "0"
  | key ->
      match List.fold_left (fun accu dict -> f1 dict key accu) [] dicts with
      | [] ->
          arg.[0] <- '4';
          arg ^ " \n"
      | cands ->
          let resp = Encode.eucjp_of_utf8 (String.concat "/" (List.map f2 cands))
          in
          String.concat "/" ["1"; resp; "\n"]
;;

let serve server dicts =
  let process_request req =
    try
      let cmd, rest = get_command req in
      match cmd with
        | Close -> `Bye, ""

        | Lookup arg ->
            let resp =
              do_lookup_cmd dicts arg Dict.find_and_append string_of_subentry in
            `Success resp, rest

        | Get_version ->
            `Success (Version.version ^ " "), rest

        | Get_address ->
            let addr = try string_of_sockaddr (getsockname server.out_fd)
                       with _ -> "" in
            let host_info =
              String.concat "" [gethostname (); ":"; addr; ":"; " "] in
            `Success host_info, rest

        | Complete arg ->
            let resp =
              do_lookup_cmd dicts arg Dict.complete_and_append (fun x -> x) in
            `Success resp, rest

        | _ -> `Success "0", rest
    with
    | Not_enough_data -> `Failure, req
  in


  let write_and_save_state s =
    let rest =
      try write_and_get_rest server.out_fd s 0 (String.length s)
      with _ -> raise End
    in
    server.wbuf <- rest;
  in

  let rec loop req =
    match process_request req with
    | `Bye, _ -> raise End
    | `Failure, rest ->
        server.rbuf <- rest;
        `Reading
    | `Success s, rest ->
        write_and_save_state s;
        match server.wbuf with
        | "" -> loop rest
        | _ ->
            server.rbuf <- rest;
            `Writing
  in

  let maxlen = 4096 in
  let buf = String.create maxlen in

  try
    if server.wbuf <> "" then
      write_and_save_state server.wbuf;

    let ret =
      try
        match read server.in_fd buf 0 maxlen with
        | 0 -> raise End
        | r -> r
      with
      | Unix_error (EWOULDBLOCK, _, _) | Unix_error (EAGAIN, _, _) -> 0
      | _ -> raise End
    in

    let req = server.rbuf ^ (String.sub buf 0 ret) in
    if String.length req > maxlen then
      raise End;

    if server.wbuf = "" then
      loop req
    else begin
      server.rbuf <- req;
      `Writing
    end

  with
  | End -> `End
  | _ ->
      prerr_endline "unexpected exception";
      `End
;;
