(*
 * encode.ml
 *      Encoding utility
 *)

module C = CamomileLibraryDefault.Camomile

let eucjp = C.CharEncoding.of_name "EUC-JP"

let utf8_of_eucjp =
  C.CharEncoding.recode_string ~in_enc:eucjp ~out_enc:C.CharEncoding.utf8

let eucjp_of_utf8 =
  C.CharEncoding.recode_string ~in_enc:C.CharEncoding.utf8 ~out_enc:eucjp
