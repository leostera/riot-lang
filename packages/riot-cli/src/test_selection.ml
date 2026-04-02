open Std

type request = {
  package_filter: string option;
  suite_filter: string option;
  query: string option;
}

let split_once = fun value ch ->
  match String.index_opt value ch with
  | None -> None
  | Some idx ->
      let left = String.sub value 0 idx in
      let right = String.sub value (idx + 1) (String.length value - idx - 1) in
      Some (left, right)

let has_text = fun value -> not (String.equal value "")

let parse_selector = fun pattern ->
  match split_once pattern ':' with
  | Some (package_name, rest) when has_text package_name && has_text rest -> (
      match split_once rest ':' with
      | Some (suite_name, query) when has_text suite_name && has_text query -> Some {
        package_filter = Some package_name;
        suite_filter = Some suite_name;
        query = Some query
      }
      | None -> Some { package_filter = Some package_name; suite_filter = Some rest; query = None }
      | _ -> None
    )
  | _ -> None

let parse_request = fun ~pattern ~legacy_package ->
  match (legacy_package, pattern) with
  | Some package_filter, _ ->
      { package_filter = Some package_filter; suite_filter = None; query = pattern }
  | None, Some pattern -> (
      match parse_selector pattern with
      | Some request -> request
      | None -> { package_filter = None; suite_filter = None; query = Some pattern }
    )
  | None, None ->
      { package_filter = None; suite_filter = None; query = None }

let extra_args = fun request args ->
  match request.query with
  | None -> args
  | Some query -> query :: args
