open Std

type t = Types.Runtime_helper.t = {
  name: string;
  symbol: string;
}

let to_json = Types.Runtime_helper.to_json

let make = fun ~name ~symbol -> Types.Runtime_helper.{ name; symbol }

let eq = make ~name:"raml_eq" ~symbol:"raml_eq"

let tuple_make = fun ~arity ->
  let symbol = format Format.[ str "raml_tuple_make_"; int arity ] in
  make ~name:symbol ~symbol

let tuple_get = make ~name:"raml_tuple_get" ~symbol:"raml_tuple_get"
