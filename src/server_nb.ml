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
  service_func : string -> (string * string) option;
  mutable rbuf : string;
  mutable wbuf : string;
}

type status =
  | Close
  | Wait_readable
  | Wait_writable


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
    | "" -> Wait_readable
    | req ->
        match server.service_func req with
        | None ->
            raise Closed

        | Some ("", rest) ->
            Wait_readable

        | Some (output, rest) ->
            match write_and_save_state output with
            | "" ->
                loop rest
            | _ ->
                server.rbuf <- rest;
                Wait_writable
  in

  try
    match if server.wbuf = "" then "" else write_and_save_state server.wbuf with
    | "" ->
        let buf = String.create buffer_length in
        let ret = read_nb in_fd buf 0 buffer_length in
        loop (server.rbuf ^ (String.sub buf 0 ret))
    | _ ->
        Wait_writable
  with
  | Closed -> Close
  | e ->
      prerr_endline (Printexc.to_string e);
      Close
;;
