(*
 * dict.ml
 *      SKK sqlite3 dictionary
 *)

open Ext

type t = Sqlite3.db

exception Error

let ng = ((<>) Sqlite3.Rc.OK)

let str_of_rc = function
  | Sqlite3.Data.TEXT s -> s
  | _ -> raise Error
;;

let rec rows_fold stmt f accu =
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW -> rows_fold stmt f (f accu (Sqlite3.row_data stmt))
  | Sqlite3.Rc.DONE -> accu
  | _ -> raise Error
;;

let opendict file = Sqlite3.db_open file

let find_and_append db key accu =
  let stmt = Sqlite3.prepare db
    "SELECT candidate, annotation FROM jisyo WHERE key = ?;" in

  if ng & Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key) then
    raise Error;

  rows_fold stmt
    (fun accu' row ->
      let cand = str_of_rc row.(0) and anno = str_of_rc row.(1) in
      let annos = if anno = "" then [] else [anno] in
      List.append_uniq_assoc (cand, annos) accu')
    accu
;;

let complete_and_append db key accu =
  let stmt = Sqlite3.prepare db
    "SELECT DISTINCT key FROM jisyo WHERE key GLOB ? AND okuri_ari = 0;" in

  if ng & Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (key ^ "*")) then
    raise Error;

  let rev_added = rows_fold stmt
    (fun accu' row ->
      let k = str_of_rc row.(0) in
      if List.mem k accu then accu' else k :: accu')
    []
  in
  List.merge String.compare accu (List.fast_sort String.compare rev_added)
;;

let add db key cand anno okuri_ari =
  let stmt = Sqlite3.prepare db
    "INSERT INTO jisyo VALUES (NULL, ?, ?, ?, ?);" in

  if ng & Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key) ||
     ng & Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT cand) ||
     ng & Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT anno) ||
     ng & Sqlite3.bind stmt 4 (Sqlite3.Data.INT (int64_of_bool okuri_ari)) ||
     Sqlite3.step stmt <> Sqlite3.Rc.DONE
  then
    raise Error;
;;

let add_from_stream db stream =
  if ng & Sqlite3.exec db "BEGIN;" then
    raise Error;

  try
    while true do
      let key, cand, anno, okuri_ari = Stream.next stream in
      add db key cand anno okuri_ari
    done
  with
  | Stream.Failure ->
      if ng & Sqlite3.exec db "COMMIT;" then
        raise Error
  | Error ->
      ignore (Sqlite3.exec db "ROLLBACK;");
      raise Error
;;

let create_table db =
  let sql =
    "CREATE TABLE jisyo (
        id INTEGER PRIMARY KEY,
        key TEXT,
        candidate TEXT,
        annotation TEXT,
        okuri_ari BOOL
    );
    CREATE INDEX keyidx ON jisyo(key);"
  in
  if ng & Sqlite3.exec db sql then
    raise Error;
;;
