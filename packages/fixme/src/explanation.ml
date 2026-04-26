open Std

type t = {
  rule_id: Rule_id.t;
  body: string;
  message: string;
}

let format = fun entry -> Rule_id.to_string entry.rule_id ^ "\n\n" ^ String.trim entry.body ^ "\n"
