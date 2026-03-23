open Std

type t = {
  rule_id : string;
  title : string;
  body : string;
  message : string;
}

val format : t -> string
