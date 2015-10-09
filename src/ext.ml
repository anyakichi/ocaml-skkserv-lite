(**
 * Extensions for standard library
 *)

external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let ( % ) f g x = f (g x)

let int64_of_bool b = if b then 1L else 0L

module List = struct
  include List

  let append_uniq l1 l2 = l1 @ filter (not % ListLabels.mem ~set:l1) l2

  let append_uniq_assoc (key, vals) l =
    let rec loop accu = function
      | [] -> (key, vals) :: accu
      | (k, vs) :: xs when key = k ->
          rev_append xs ((key, append_uniq vs vals) :: accu)
      | x :: xs ->
          loop (x :: accu) xs
    in
    rev (loop [] l)
  ;;
end

module ListLabels = struct
  include ListLabels

  let append_uniq = List.append_uniq
  let append_uniq_assoc = List.append_uniq_assoc
end


module String = struct
  include String

  let lstrip s =
    let isspace c = c = ' ' || c = '\n' || c = '\r' || c = '\t' in
    let len = String.length s in
    let rec loop i =
      if i < len && isspace s.[i] then
        loop (i + 1)
      else
        i
    in
    let i = loop 0 in
    String.sub s i (len - i)
  ;;

  let split2 s sep =
    let len = String.length s in
    let i = String.index s sep in
    (String.sub s 0 i, String.sub s (i + 1) (len - i - 1))
  ;;

  let split2_silent s sep =
    try split2 s sep with Not_found -> (s, "")
  ;;
end

module StringLabels = struct
  include StringLabels

  let lstrip = String.lstrip
  let split2 s ~on = String.split2 s on
  let split2_silent s ~on = String.split2_silent s on
end


module StdLabels = struct
  module Array = ArrayLabels
  (*module Bytes = BytesLabels*)
  module List = ListLabels
  module String = StringLabels
end


module Unix = struct
  include Unix

  let string_of_sockaddr = function
    | ADDR_UNIX s -> s
    | ADDR_INET (in_addr, port) ->
        let s = string_of_inet_addr in_addr in
        if String.contains s ':' then     (* IPv6 address *)
          String.concat "" ["["; s; "]"]
        else
          s
  ;;
end
