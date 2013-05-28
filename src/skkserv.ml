(*
 * skkserv.ml
 *      SKK server services
 *)

open Ext

open Unix

exception End
exception Error
exception Exit
exception Not_enough_arguments of string

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
  | Continue
  | Unknown


let string_of_subentry = function
  | cand, [] -> cand
  | cand, annos -> String.concat ";" [cand; String.concat "," annos]
;;

let create ~in_fd ~out_fd =
  { in_fd = in_fd; out_fd = out_fd; rbuf = ""; wbuf = "" }

let get_command req =
  let cmd = req.[0] in
  let rest = Str.string_after req 1 in

  let cmd1 f s =
    match Str.bounded_split_delim (Str.regexp "[ \t\r\n]+") s 2 with
    | [arg; rest] -> f arg, rest
    | _ -> Continue, req
  in

  match cmd with
  | '0' -> Close, rest
  | '1' -> cmd1 (fun x -> Lookup x) rest
  | '2' -> Get_version, rest
  | '3' -> Get_address, rest
  | '4' -> cmd1 (fun x -> Complete x) rest
  | _ -> Unknown, rest
;;

let write_nb fd buf off len =
  try write fd buf off len
  with Unix_error (EWOULDBLOCK, _, _) | Unix_error (EAGAIN, _, _) -> 0
;;

let write_and_get_rest fd buf off len =
  let len' = write_nb fd buf off len in
  String.sub buf len' (len - len')

let do_lookup_cmd dicts arg f1 f2 =
  let key = Encode.utf8_of_eucjp arg in
  match List.fold_left (fun accu dict -> f1 dict key accu) [] dicts with
  | [] ->
      arg.[0] <- '4';
      arg ^ " \n"
  | cands ->
      let resp = Encode.eucjp_of_utf8 (String.concat "/" (List.map f2 cands)) in
      String.concat "/" ["1"; resp; "\n"]
;;

let serve server dicts =
  let maxlen = 4096 in
  let buf = String.create maxlen in

  let write_and_save_state s =
    let rest =
      try write_and_get_rest server.out_fd s 0 (String.length s)
      with _ -> raise End
    in
    server.wbuf <- rest;
  in

  let rec process_request req =
    try
      match req with
      | "" -> `Reading
      | _ ->
          let cmd, rest = get_command req in
          let resp = match cmd with
            | Close ->
                raise End

            | Lookup arg ->
                do_lookup_cmd dicts arg Dict.find_and_append string_of_subentry

            | Get_version ->
                Version.version ^ " "

            | Get_address ->
                let addr = try string_of_sockaddr (getsockname server.out_fd)
                          with _ -> "" in
                String.concat "" [gethostname (); ":"; addr; ":"; " "]

            | Complete arg ->
                do_lookup_cmd dicts arg Dict.complete_and_append (fun x -> x)

            | Continue ->
                server.rbuf <- req;
                raise Exit

            | _ -> "0"
          in
          write_and_save_state resp;
          match server.wbuf with
          | "" -> process_request rest
          | _ ->
              server.rbuf <- rest;
              `Writing
    with Exit -> `Reading
  in

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

    match server.wbuf with
    | "" -> process_request req
    | _ ->
        server.rbuf <- req;
        `Writing

  with
  | End -> `End
  | _ ->
      prerr_endline "unexpected exception";
      `End
;;
