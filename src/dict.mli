(*
 * dict.mli
 *      SKK sqlite3 dictionary
 *)

type t

val opendict : string -> t

val find_and_append :
  t -> string -> (string * string list) list -> (string * string list) list
val complete_and_append : t -> string -> string list -> string list

val add : t -> string -> string -> string -> bool -> unit
val add_from_stream : t -> (string * string * string * bool) Stream.t -> unit

val create_table : t -> unit
