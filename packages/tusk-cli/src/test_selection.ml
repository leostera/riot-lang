open Std

type suite = {
  package_name : string;
  suite_name : string;
  case_names : string list;
}

type request =
  | All
  | Query of string
  | PackageAll of string
  | PackageQuery of {
      package_name : string;
      query : string;
    }

type selection =
  | RunSuite of suite
  | RunCases of {
      suite : suite;
      query : string;
      matched_cases : string list;
    }

let suite_identity suite = suite.package_name ^ ":" ^ suite.suite_name

let parse_request ~pattern ~legacy_package =
  match (pattern, legacy_package) with
  | (Some raw_pattern, _) -> (
      match String.split_on_char ':' raw_pattern with
      | [ package_name; "..." ] -> PackageAll package_name
      | [ package_name; query ] -> PackageQuery { package_name; query }
      | _ -> Query raw_pattern)
  | (None, Some package_name) -> PackageAll package_name
  | (None, None) -> All

let package_filter = function
  | PackageAll package_name -> Some package_name
  | PackageQuery { package_name; _ } -> Some package_name
  | All | Query _ -> None

let matches_query ~query value = String.contains value query

let suite_matches_query ~query suite =
  matches_query ~query suite.package_name
  || matches_query ~query suite.suite_name
  || matches_query ~query (suite_identity suite)

let matched_case_names ~query suite =
  List.filter (fun case_name -> matches_query ~query case_name) suite.case_names

let select_one request suite =
  match request with
  | All -> Some (RunSuite suite)
  | PackageAll package_name ->
      if String.equal suite.package_name package_name then Some (RunSuite suite)
      else None
  | Query query ->
      if suite_matches_query ~query suite then Some (RunSuite suite)
      else (
        match matched_case_names ~query suite with
        | [] -> None
        | matched_cases -> Some (RunCases { suite; query; matched_cases }))
  | PackageQuery { package_name; query } ->
      if not (String.equal suite.package_name package_name) then None
      else if String.equal query "" then Some (RunSuite suite)
      else if matches_query ~query suite.suite_name then Some (RunSuite suite)
      else (
        match matched_case_names ~query suite with
        | [] -> None
        | matched_cases -> Some (RunCases { suite; query; matched_cases }))

let select request suites = List.filter_map (select_one request) suites
