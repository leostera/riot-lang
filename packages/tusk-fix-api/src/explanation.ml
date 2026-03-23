open Std

type t = {
  rule_id : string;
  title : string;
  body : string;
  message : string;
}

let format entry =
  entry.title ^ "\n\n" ^ entry.body ^ "\n"
