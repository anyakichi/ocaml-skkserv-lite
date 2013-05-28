(*
 * skkserv.ml
 *      SKK server services
 *)

open Ext

open Unix

exception Close
exception Server_error
exception Server_not_found

let string_of_subentry = function
  | cand, [] -> cand
  | cand, annos -> String.concat ";" [cand; String.concat "," annos]
;;

let write' fd data offs len =
  let ret = write fd data offs len in
  if ret = len then `Success else `Failure
;;

let serve ~in_fd ~out_fd dicts =
  let maxlen = 512 in
  let buf = String.create maxlen in

  let len = ref (try read in_fd buf 0 maxlen with _ -> 0) in

  let respond_to_lookup_request f1 f2 =
    let key = Encode.utf8_of_eucjp (String.sub buf 1 (!len - 2)) in
    let result = List.fold_left (fun accu dict -> f1 dict key accu) [] dicts in

    if result = [] then
      raise Server_not_found;

    let result' = List.map f2 result in
    let rep = Encode.eucjp_of_utf8 (String.concat "/" result') in
    let rep' = String.concat "/" ["1"; rep; "\n"] in
    write' out_fd rep' 0 (String.length rep')
  in

  try
    if !len = 0 then
      raise Close;

    while buf.[!len - 1] = '\n' || buf.[!len - 1] = '\r' do
      len := !len - 1
    done;

    if !len = 0 then
      raise Server_error;

    match buf.[0] with
    | '0' ->
        raise Close
    | '1' | '4' when buf.[!len - 1] <> ' ' ->
        raise Server_not_found
    | '1' ->
        respond_to_lookup_request Dict.find_and_append string_of_subentry
    | '2' ->
        let version = Version.version ^ " " in
        write' out_fd version 0 (String.length version)
    | '3' ->
        let addr = try string_of_sockaddr (getsockname out_fd) with _ -> "" in
        let host_info = String.concat "" [gethostname (); ":"; addr; ":"; " "]
        in
        write' out_fd host_info 0 (String.length host_info)
    | '4' ->
        respond_to_lookup_request Dict.complete_and_append (fun x -> x)
    | _ ->
        raise Server_error

  with
  | Close ->
      close out_fd;
      `Closed
  | Server_error ->
      write' out_fd "0" 0 1
  | Server_not_found ->
      let resp = String.sub buf 0 !len in
      resp.[0] <- '4';
      write' out_fd (resp ^ "\n") 0 (!len + 1)
;;
