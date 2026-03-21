open Std

type t = {
  code : string;
  rule_id : string;
  title : string;
  body : string;
  message : string;
}

val format : t -> string
