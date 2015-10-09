(**
 * skkserv main routine
 *)

open Ext
open StdLabels
open Unix

exception End
exception Not_enough_data

type t = {
  fd : file_descr;
  ref_dicts : Dict.t list ref;
}

type command =
  | Close
  | Lookup of string
  | Get_version
  | Get_address
  | Complete of string
  | Unknown


let create ~fd ref_dicts =
  { fd; ref_dicts }
;;

(** [get_command req] parses request [req] and returns the first command in
    [req] and rest of [req].
    Raises Not_enough_data when [req] is not enough to parse and is not
    consumed. *)
let get_command req =
  let get_arg s =
    try String.split2 s ~on:' ' with Not_found -> raise Not_enough_data
  in

  if req = "" then
    raise Not_enough_data;

  let cmd = req.[0] in
  let rest = Str.string_after req 1 in

  match cmd with
  | '0' -> Close, rest
  | '1' ->
      let arg, rest' = get_arg rest in
      Lookup arg, rest'
  | '2' -> Get_version, rest
  | '3' -> Get_address, rest
  | '4' ->
      let arg, rest' = get_arg rest in
      Complete arg, rest'
  | _ -> Unknown, rest
;;

let do_lookup_cmd dicts arg f1 f2 =
  match Encode.utf8_of_eucjp arg with
  | "" -> "0"
  | key ->
      match List.fold_left ~f:(fun accu dict -> f1 dict key accu) ~init:[]
              dicts with
      | [] ->
          String.concat "" ["4"; arg; " \n"]
      | cands ->
          let resp = String.concat "/" (List.map f2 cands) in
          String.concat "/" ["1"; Encode.eucjp_of_utf8 resp; "\n"]
;;

let serve t input =
  let string_of_entry = function
    | cand, [] -> cand
    | cand, annos -> String.concat ";" [cand; String.concat "," annos]
  in

  let dicts = !(t.ref_dicts) in
  try
    let input = String.lstrip input in

    (* force close session for too long request. *)
    if String.length input > 4096 then
      raise End;

    let cmd, rest = get_command input in
    let resp = match cmd with
      | Close ->
          raise End;

      | Lookup arg ->
          do_lookup_cmd dicts arg Dict.find_and_append string_of_entry

      | Get_version ->
          Version.version ^ " "

      | Get_address ->
          let addr = try string_of_sockaddr (getsockname t.fd)
                     with _ -> "" in
          String.concat "" [gethostname (); ":"; addr; ":"; " "]

      | Complete arg ->
          do_lookup_cmd dicts arg Dict.complete_and_append (fun x -> x)

      | _ -> "0"
    in
    Some (resp, rest)
  with
  | End -> None
  | Not_enough_data -> Some ("", input)
;;
