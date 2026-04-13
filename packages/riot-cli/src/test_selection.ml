open Std

type size_filter =
  | All
  | Small
  | Large

type request = {
  package_filter: string option;
  suite_filter: string option;
  query: string option;
  size_filter: size_filter;
  flaky_only: bool;
}

let split_once = fun value ch ->
  let rec find at =
    if at >= String.length value then
      None
    else if Char.equal (String.get_unchecked value ~at) ch then
      Some at
    else
      find (at + 1)
  in
  match find 0 with
  | None -> None
  | Some idx ->
      let left = String.sub value ~offset:0 ~len:idx in
      let right = String.sub value ~offset:(idx + 1) ~len:(String.length value - idx - 1) in
      Some (left, right)

let has_text = fun value -> not (String.equal value "")

let parse_selector = fun pattern ->
  match split_once pattern ':' with
  | Some (package_name, rest) when has_text package_name && has_text rest -> (
      match split_once rest ':' with
      | Some (suite_name, query) when has_text suite_name && has_text query -> Some (
        Some package_name,
        Some suite_name,
        Some query
      )
      | None -> Some (Some package_name, Some rest, None)
      | _ -> None
    )
  | _ -> None

let parse_request = fun ~pattern ~legacy_package ~size_filter ~flaky_only ->
  match (legacy_package, pattern) with
  | Some package_filter, _ ->
      {
        package_filter = Some package_filter;
        suite_filter = None;
        query = pattern;
        size_filter;
        flaky_only;
      }
  | None, Some pattern -> (
      match parse_selector pattern with
      | Some (package_filter, suite_filter, query) ->
          {
            package_filter;
            suite_filter;
            query;
            size_filter;
            flaky_only;
          }
      | None ->
          {
            package_filter = None;
            suite_filter = None;
            query = Some pattern;
            size_filter;
            flaky_only;
          }
    )
  | None, None ->
      {
        package_filter = None;
        suite_filter = None;
        query = None;
        size_filter;
        flaky_only;
      }

let extra_args = fun ?(small_test_timeout = None) ?(flaky_max_retries = 0) request args ->
  let selection_args =
    let query_args =
      match request.query with
      | None -> []
      | Some query -> [ query ]
    in
    let size_args =
      match request.size_filter with
      | All -> []
      | Small -> [ "--small" ]
      | Large -> [ "--large" ]
    in
    let flaky_args =
      if request.flaky_only then
        [ "--flaky" ]
      else
        []
    in
    query_args @ size_args @ flaky_args
  in
  let policy_args =
    let timeout_args =
      match small_test_timeout with
      | Some timeout -> [ "--small-timeout-ms"; Int.to_string (Time.Duration.to_millis timeout) ]
      | None -> []
    in
    let retry_args =
      if flaky_max_retries > 0 then
        [ "--flaky-max-retries"; Int.to_string flaky_max_retries ]
      else
        []
    in
    timeout_args @ retry_args
  in
  selection_args @ policy_args @ args
