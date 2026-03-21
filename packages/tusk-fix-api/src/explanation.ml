open Std

type t = {
  code : string;
  rule_id : string;
  title : string;
  body : string;
  message : string;
}

let format entry =
  entry.code ^ " - " ^ entry.title ^ "\n\n" ^ entry.body ^ "\n"
