(*
 * skkserv.ml
 *      SKK server services
 *)

open Ext
open Server_nb

open Unix

exception End
exception Not_enough_data

type t = {
  socket : file_descr;
  ref_dicts : Dict.t list ref;
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

let create ~socket ref_dicts =
  { socket = socket; ref_dicts = ref_dicts }
;;

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
          let resp = String.concat "/" (List.map f2 cands) in
          String.concat "/" ["1"; Encode.eucjp_of_utf8 resp; "\n"]
;;

let serve server input =
  let dicts = !(server.ref_dicts) in
  try
    if String.length input > 4096 then
      raise End;

    let cmd, rest = get_command input in
    let resp = match cmd with
      | Close ->
          raise End;

      | Lookup arg ->
          do_lookup_cmd dicts arg Dict.find_and_append string_of_subentry

      | Get_version ->
          Version.version ^ " "

      | Get_address ->
          let addr = try string_of_sockaddr (getsockname server.socket)
                     with _ -> "" in
          String.concat "" [gethostname (); ":"; addr; ":"; " "]

      | Complete arg ->
          do_lookup_cmd dicts arg Dict.complete_and_append (fun x -> x)

      | _ -> "0"
    in
    Some resp, Ready rest
  with
  | End -> None, Ready ""
  | Not_enough_data -> Some "", Not_ready input
;;
