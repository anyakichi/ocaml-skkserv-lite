(*
 * server_nb.ml
 *      Non-blocking server support
 *)

open Unix

exception Closed

type ready =
  | Ready of string
  | Not_ready of string

type t = {
  service_func : string -> string option * ready;
  mutable rbuf : string;
  mutable wbuf : string;
}


let buffer_length = 4096

let read_nb fd buf off len =
  try
    match read fd buf off len with
    | 0 -> raise Closed
    | n -> n
  with
  | Unix_error (EWOULDBLOCK, _, _) | Unix_error (EAGAIN, _, _) -> 0
;;

let write_nb fd buf off len =
  try write fd buf off len
  with
  | Unix_error (EWOULDBLOCK, _, _) | Unix_error (EAGAIN, _, _) -> 0
  | Unix_error (EPIPE, _, _) -> raise Closed
;;

let write_and_get_rest fd buf off len =
  let len' = write_nb fd buf off len in
  String.sub buf len' (len - len')
;;

let create f =
  { service_func = f; rbuf = ""; wbuf = "" }
;;

let serve server ~in_fd ~out_fd =
  let write_and_save_state s =
    let rest = write_and_get_rest out_fd s 0 (String.length s) in
    server.wbuf <- rest;
    rest
  in

  let rec loop = function
    | "" -> `Reading
    | req ->
        match server.service_func req with
        | None, _ ->
            raise Closed

        | Some "", Not_ready rest ->
            `Reading

        | Some output, Not_ready rest ->
            server.rbuf <- rest;
            if write_and_save_state output = "" then `Reading else `Writing

        | Some output, Ready rest ->
            match write_and_save_state output with
            | "" ->
                loop rest
            | _ ->
                server.rbuf <- rest;
                `Writing
  in

  try
    let buf = String.create buffer_length in
    match server.wbuf with
    | "" ->
        let ret = read_nb in_fd buf 0 buffer_length in
        loop (server.rbuf ^ (String.sub buf 0 ret))
    | s ->
        if write_and_save_state s = "" then `Reading else `Writing
  with
  | Closed -> `End
  | e ->
      prerr_endline (Printexc.to_string e);
      `End
;;
