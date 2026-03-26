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

val suite_identity : suite -> string
val parse_request : pattern:string option -> legacy_package:string option -> request
val package_filter : request -> string option
val select : request -> suite list -> selection list
