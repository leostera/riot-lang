open Std

type suite = {
  package_name : string;
  suite_name : string;
}

type request =
  | All
  | Query of string
  | PackageAll of string
  | PackageQuery of {
      package_name : string;
      query : string;
    }

type execution = RunSuite | RunQuery of string

val suite_identity : suite -> string
val parse_request : pattern:string option -> legacy_package:string option -> request
val package_filter : request -> string option
val execution_for_suite : request -> suite -> execution option
