open Std

val list_rules_output: format:Reporter.format -> string

val list_diagnostics_output: format:Reporter.format -> string

val list_rules: Reporter.format -> (unit, exn) result

val list_diagnostics: Reporter.format -> (unit, exn) result

val explain_rule: string -> (unit, exn) result
