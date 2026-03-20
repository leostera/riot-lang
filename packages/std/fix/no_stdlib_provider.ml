open Std

let package_rule_id = "pkg:no-stdlib"

let unix_code =
  Tusk_fix.Diagnostic_code.
    {
      id = "FSTD0001";
      rule_id = package_rule_id;
      title = "Direct Unix usage";
      body = Tusk_fix.Diagnostic_code.body DirectUnixUsage;
      message = Tusk_fix.Diagnostic_code.message DirectUnixUsage;
    }

let sys_code =
  Tusk_fix.Diagnostic_code.
    {
      id = "FSTD0002";
      rule_id = package_rule_id;
      title = "Direct Sys usage";
      body = Tusk_fix.Diagnostic_code.body DirectSysUsage;
      message = Tusk_fix.Diagnostic_code.message DirectSysUsage;
    }

let stdlib_code =
  Tusk_fix.Diagnostic_code.
    {
      id = "FSTD0003";
      rule_id = package_rule_id;
      title = "Direct Stdlib usage";
      body = Tusk_fix.Diagnostic_code.body DirectStdlibUsage;
      message = Tusk_fix.Diagnostic_code.message DirectStdlibUsage;
    }

let pervasives_code =
  Tusk_fix.Diagnostic_code.
    {
      id = "FSTD0004";
      rule_id = package_rule_id;
      title = "Direct Pervasives usage";
      body = Tusk_fix.Diagnostic_code.body DirectPervasivesUsage;
      message = Tusk_fix.Diagnostic_code.message DirectPervasivesUsage;
    }

let diagnostic_codes () =
  [ unix_code; sys_code; stdlib_code; pervasives_code ]

let package_code_for_builtin = function
  | Tusk_fix.Diagnostic_code.DirectUnixUsage -> unix_code
  | DirectSysUsage -> sys_code
  | DirectStdlibUsage -> stdlib_code
  | DirectPervasivesUsage -> pervasives_code
  | PackageProvided entry -> entry

let remap_diagnostic diag =
  let kind =
    match Tusk_fix.Diagnostic.code diag with
    | Some code ->
        Tusk_fix.Diagnostic.Known
          (Tusk_fix.Diagnostic_code.PackageProvided (package_code_for_builtin code))
    | None ->
        Tusk_fix.Diagnostic.Generic
          {
            rule_id = package_rule_id;
            message = Tusk_fix.Diagnostic.message diag;
          }
  in
  Tusk_fix.Diagnostic.make
    ~severity:(Tusk_fix.Diagnostic.severity diag)
    ~kind ~span:(Tusk_fix.Diagnostic.span diag)
    ?suggestion:(Tusk_fix.Diagnostic.suggestion diag)
    ?fix:(Tusk_fix.Diagnostic.fix diag) ()

let run_builtin ctx tree =
  let builtin = Tusk_fix.Rules.No_stdlib.make () in
  Tusk_fix.Rule.run builtin ctx tree
  |> List.map remap_diagnostic

let rules () =
  [
    Tusk_fix.Rule.make ~id:package_rule_id ~name:"No OCaml Stdlib"
      ~description:
        "Detects direct Stdlib, Unix, Sys, and Pervasives usage from the Std package boundary"
      ~run:run_builtin ();
  ]

let name = "std"
