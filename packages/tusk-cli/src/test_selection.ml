open Std

type request = {
  package_filter: string option;
  query: string option;
}

let parse_request = fun ~pattern ~legacy_package -> {
  package_filter = legacy_package;
  query = pattern
}

let extra_args = fun request args ->
  match request.query with
  | None -> args
  | Some query -> query :: args
