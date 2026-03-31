open Std

type t = {
  rule_id: string;
  body: string;
  message: string;
}

let format = fun entry -> entry.rule_id ^ "\n\n" ^ String.trim entry.body ^ "\n"
