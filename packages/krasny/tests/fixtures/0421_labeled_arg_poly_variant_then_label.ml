open Std

let parse_interface ~source tokens =
  parse ~cst_kind:`Interface ~parse_item:parse_signature_item ~source ~tokens
