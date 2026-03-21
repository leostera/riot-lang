open Std

let write_file path content =
  Fs.write content path |> Result.expect ~msg:"failed to write test fixture"

let read_file path =
  Fs.read path |> Result.expect ~msg:"failed to read test fixture"

let run_cli argv =
  match ArgParser.get_matches Tusk_fix.Cli.command ("fix" :: argv) with
  | Error err -> Error (Failure (ArgParser.error_message err))
  | Ok matches -> Tusk_fix.Cli.run matches

let with_cwd path fn =
  let original =
    Env.current_dir () |> Result.expect ~msg:"failed to get cwd"
  in
  Env.set_current_dir path |> Result.expect ~msg:"failed to chdir into test dir";
  try
    let result = fn () in
    Env.set_current_dir original
    |> Result.expect ~msg:"failed to restore cwd";
    result
  with exn ->
    Env.set_current_dir original
    |> Result.expect ~msg:"failed to restore cwd after exception";
    raise exn

let with_tempdir prefix fn =
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let diagnostic_codes diagnostics =
  diagnostics
  |> List.filter_map Tusk_fix.Diagnostic.code
  |> List.sort String.compare

let assert_explanation_contains ~code ~snippet =
  match Tusk_fix.Explanations.explain code with
  | None -> Error ("Expected explanation for " ^ code)
  | Some entry ->
      Test.assert_equal ~expected:code ~actual:entry.code;
      Test.assert_true (String.contains entry.body snippet);
      Ok ()

let tests =
  [
    Test.case "snake-case-type-names exposes safe fixes" (fun () ->
        let source = "type userProfile = { name : string }\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let fixes =
          List.filter_map Tusk_fix.Diagnostic.fix result.diagnostics
        in
        Test.assert_equal ~expected:1 ~actual:(List.length fixes);
        Ok ());
    Test.case "snake-case-type-names keeps compliant type names clean" (fun () ->
        let source = "type user_profile = { name : string }\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "snake-case-type-names emits stable diagnostic codes" (fun () ->
        let source = "type userProfile = int\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0101" ] ~actual:codes;
        Ok ());
    Test.case "descriptive-type-variables flags short type parameters" (fun () ->
        let source = "type ('a, 'error) resultish = ('a, 'error) result\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0102" ] ~actual:codes;
        Ok ());
    Test.case "descriptive-type-variables keeps descriptive type parameters clean" (fun () ->
        let source =
          "type ('value, 'error) resultish = ('value, 'error) result\n"
        in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "descriptive-type-variables ignores nested type variable usages" (fun () ->
        let source = "type 'value callback = 'a -> 'value\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains type-name violations" (fun () ->
        assert_explanation_contains ~code:"F0101" ~snippet:"snake_case");
    Test.case "diagnostic code registry explains short type variables" (fun () ->
        assert_explanation_contains ~code:"F0102" ~snippet:"'value");
    Test.case "snake-case-function-names flags camelCase function bindings" (fun () ->
        let source = "let userProfile x = x\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0103" ] ~actual:codes;
        Ok ());
    Test.case "snake-case-function-names flags explicit fun bindings" (fun () ->
        let source = "let userProfile = fun x -> x\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:1
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "snake-case-function-names keeps compliant function names clean" (fun () ->
        let source = "let user_profile x = x\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "snake-case-function-names ignores camelCase value bindings" (fun () ->
        let source = "let userProfile = 42\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Snake_case_function_names.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[] ~actual:codes;
        Ok ());
    Test.case "snake-case-function-names flags local camelCase function bindings" (fun () ->
        let source = "let render x = let userProfile y = y in userProfile x\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0103" ] ~actual:codes;
        Ok ());
    Test.case "diagnostic code registry explains function-name violations" (fun () ->
        assert_explanation_contains ~code:"F0103" ~snippet:"parse_user");
    Test.case "class-case-module-names flags jiraffe-cased modules" (fun () ->
        let source = "module Foo_bar = struct end\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0104" ] ~actual:codes;
        Ok ());
    Test.case "class-case-module-names flags jiraffe-cased module types" (fun () ->
        let source = "module type Foo_bar = sig end\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:1
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "class-case-module-names keeps ClassCased modules clean" (fun () ->
        let source = "module FooBar = struct end\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains module-name violations" (fun () ->
        assert_explanation_contains ~code:"F0104" ~snippet:"FooBar");
    Test.case "snake-case-variable-names flags camelCase value bindings" (fun () ->
        let source = "let currentUser = 42\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0105" ] ~actual:codes;
        Ok ());
    Test.case "snake-case-variable-names flags local camelCase value bindings" (fun () ->
        let source = "let render x = let currentUser = x in currentUser\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0105" ] ~actual:codes;
        Ok ());
    Test.case "snake-case-variable-names keeps compliant values clean" (fun () ->
        let source = "let current_user = 42\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "snake-case-variable-names ignores camelCase function bindings" (fun () ->
        let source = "let currentUser x = x\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:1
          ~actual:(List.length result.diagnostics);
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0103" ] ~actual:codes;
        Ok ());
    Test.case "diagnostic code registry explains variable-name violations" (fun () ->
        assert_explanation_contains ~code:"F0105" ~snippet:"current_user");
    Test.case "no-prime-variables flags prime-suffixed value bindings" (fun () ->
        let source = "let current_user' = 42\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0106" ] ~actual:codes;
        Ok ());
    Test.case "no-prime-variables flags local prime-suffixed value bindings" (fun () ->
        let source = "let render x = let state' = x in state'\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0106" ] ~actual:codes;
        Ok ());
    Test.case "no-prime-variables keeps non-prime values clean" (fun () ->
        let source = "let current_user2 = 42\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.No_prime_variables.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "no-prime-variables ignores prime-suffixed function bindings" (fun () ->
        let source = "let current_user' x = x\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.No_prime_variables.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains prime-variable violations" (fun () ->
        assert_explanation_contains ~code:"F0106" ~snippet:"state2");
    Test.case "snake-case-argument-names flags camelCase positional arguments" (fun () ->
        let source = "let render userId = userId\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0110" ] ~actual:codes;
        Ok ());
    Test.case "snake-case-argument-names flags camelCase labeled arguments" (fun () ->
        let source = "let render ~displayName current_user = current_user\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0110" ] ~actual:codes;
        Ok ());
    Test.case "snake-case-argument-names flags camelCase optional arguments" (fun () ->
        let source = "let render ?pageSize current_user = current_user\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0110" ] ~actual:codes;
        Ok ());
    Test.case "snake-case-argument-names keeps compliant arguments clean" (fun () ->
        let source = "let render ~display_name ?page_size current_user = current_user\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Snake_case_argument_names.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains argument-name violations" (fun () ->
        assert_explanation_contains ~code:"F0110" ~snippet:"display_name");
    Test.case "ordered-argument-kinds flags labeled arguments after positional ones" (fun () ->
        let source = "let render current_user ~display_name = current_user\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Ordered_argument_kinds.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0111" ] ~actual:codes;
        Ok ());
    Test.case "ordered-argument-kinds flags optional arguments after positional ones" (fun () ->
        let source = "let render current_user ?page_size = current_user\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Ordered_argument_kinds.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0111" ] ~actual:codes;
        Ok ());
    Test.case "ordered-argument-kinds flags labeled arguments after optional ones" (fun () ->
        let source = "let render ?page_size ~display_name current_user = current_user\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Ordered_argument_kinds.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0111" ] ~actual:codes;
        Ok ());
    Test.case "ordered-argument-kinds keeps compliant argument order clean" (fun () ->
        let source = "let render ~display_name ?page_size current_user = current_user\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Ordered_argument_kinds.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "ordered-argument-kinds reports only one issue per function" (fun () ->
        let source = "let render current_user ~display_name ?page_size = current_user\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Ordered_argument_kinds.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:1
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains argument-order violations" (fun () ->
        assert_explanation_contains ~code:"F0111" ~snippet:"labeled arguments");
    Test.case "alphabetized-named-arguments flags unsorted labeled arguments" (fun () ->
        let source = "let render ~zebra ~alpha current_user = current_user\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Alphabetized_named_arguments.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0113" ] ~actual:codes;
        Ok ());
    Test.case "alphabetized-named-arguments flags unsorted optional arguments" (fun () ->
        let source = "let render ?zebra ?alpha current_user = current_user\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Alphabetized_named_arguments.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0113" ] ~actual:codes;
        Ok ());
    Test.case "alphabetized-named-arguments keeps each kind group independent" (fun () ->
        let source = "let render ~zebra ?alpha current_user = current_user\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Alphabetized_named_arguments.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "alphabetized-named-arguments reports one issue per function" (fun () ->
        let source = "let render ~zebra ~alpha ~beta current_user = current_user\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Alphabetized_named_arguments.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:1
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains named-argument sorting violations" (fun () ->
        assert_explanation_contains ~code:"F0113" ~snippet:"Alphabetical order");
    Test.case "t-first-named-arguments flags t after other positional arguments" (fun () ->
        let source = "let render ~width ~height other t = t\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.T_first_named_arguments.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0112" ] ~actual:codes;
        Ok ());
    Test.case "t-first-named-arguments keeps t-first positional arguments clean" (fun () ->
        let source = "let render ~width ~height t other = t\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.T_first_named_arguments.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "t-first-named-arguments ignores functions without named arguments" (fun () ->
        let source = "let render other t = t\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.T_first_named_arguments.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "t-first-named-arguments ignores functions without positional t" (fun () ->
        let source = "let render ~width other current = current\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.T_first_named_arguments.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains t-first named argument violations" (fun () ->
        assert_explanation_contains ~code:"F0112" ~snippet:"receiver");
    Test.case "snake-case-record-fields flags camelCase record fields" (fun () ->
        let source = "type user = { displayName : string; created_at : int }\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Snake_case_record_fields.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0114" ] ~actual:codes;
        Ok ());
    Test.case "snake-case-record-fields keeps snake_case fields clean" (fun () ->
        let source = "type user = { display_name : string; created_at : int }\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Snake_case_record_fields.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains record-field violations" (fun () ->
        assert_explanation_contains ~code:"F0114" ~snippet:"display_name");
    Test.case "class-case-constructors flags underscored constructors" (fun () ->
        let source = "type user = | Guest_user | RegisteredUser\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Class_case_constructors.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0115" ] ~actual:codes;
        Ok ());
    Test.case "class-case-constructors keeps ClassCased constructors clean" (fun () ->
        let source = "type user = | GuestUser | RegisteredUser\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Class_case_constructors.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains constructor-name violations" (fun () ->
        assert_explanation_contains ~code:"F0115" ~snippet:"GuestUser");
    Test.case "snake-case-polyvariant-tags flags non-snake-case tags" (fun () ->
        let source = "type user = [ `GuestUser | `registered_user ]\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Snake_case_polyvariant_tags.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0116" ] ~actual:codes;
        Ok ());
    Test.case "snake-case-polyvariant-tags keeps snake_case tags clean" (fun () ->
        let source = "type user = [ `guest_user | `registered_user ]\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Snake_case_polyvariant_tags.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains polyvariant-tag violations" (fun () ->
        assert_explanation_contains ~code:"F0116" ~snippet:"guest_user");
    Test.case "avoid-single-letter-function-names flags placeholder bindings" (fun () ->
        let source = "let f x = x\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0117" ] ~actual:codes;
        Ok ());
    Test.case "avoid-single-letter-function-names flags local placeholder bindings" (fun () ->
        let source = "let render x = let g y = y in g x\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0117" ] ~actual:codes;
        Ok ());
    Test.case "avoid-single-letter-function-names keeps descriptive bindings clean" (fun () ->
        let source = "let render_user x = x\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "avoid-single-letter-function-names ignores placeholder value bindings" (fun () ->
        let source = "let f = 42\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Avoid_single_letter_function_names.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains single-letter function bindings" (fun () ->
        assert_explanation_contains ~code:"F0117" ~snippet:"Placeholder names");
    Test.case "avoid-single-letter-type-names flags placeholder type names" (fun () ->
        let source = "type x = int\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0118" ] ~actual:codes;
        Ok ());
    Test.case "avoid-single-letter-type-names allows t" (fun () ->
        let source = "type t = int\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Avoid_single_letter_type_names.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "avoid-single-letter-type-names keeps descriptive names clean" (fun () ->
        let source = "type user_profile = int\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains single-letter type names" (fun () ->
        assert_explanation_contains ~code:"F0118" ~snippet:"conventional `t`");
    Test.case "prefer-multiline-string-literals flags chained string literals" (fun () ->
        let source = "let banner = \"hello \" ^ \"world\" ^ \"!\"\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Prefer_multiline_string_literals.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0119" ] ~actual:codes;
        Ok ());
    Test.case "prefer-multiline-string-literals ignores mixed concatenations" (fun () ->
        let source = "let banner name = \"hello \" ^ name\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Prefer_multiline_string_literals.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains multiline string preference" (fun () ->
        assert_explanation_contains ~code:"F0119" ~snippet:"multiline literal");
    Test.case "no-custom-operators flags symbolic custom operators" (fun () ->
        let source = "let composed = f %> g\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.No_custom_operators.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0120" ] ~actual:codes;
        Ok ());
    Test.case "no-custom-operators allows builtin operators" (fun () ->
        let source = "let sum = a + b\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.No_custom_operators.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains custom operators" (fun () ->
        assert_explanation_contains ~code:"F0120" ~snippet:"hard to search");
    Test.case "no-inline-parameter-type-annotations flags typed positional parameters" (fun () ->
        let source = "let render (user_id : int) (enabled : bool) = user_id\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.No_inline_parameter_type_annotations.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0121" ] ~actual:codes;
        Ok ());
    Test.case "no-inline-parameter-type-annotations keeps unsigned parameters clean" (fun () ->
        let source = "let render user_id enabled = user_id\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.No_inline_parameter_type_annotations.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains inline parameter annotations" (fun () ->
        assert_explanation_contains ~code:"F0121" ~snippet:"Function signatures");
    Test.case "no-function-shorthand flags named function shorthand" (fun () ->
        let source = "let render = function | x -> x + 1\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.No_function_shorthand.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[ "F0122" ] ~actual:codes;
        Ok ());
    Test.case "no-function-shorthand keeps fun expressions clean" (fun () ->
        let source = "let render = fun x -> x + 1\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.No_function_shorthand.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "diagnostic code registry explains function shorthand" (fun () ->
        assert_explanation_contains ~code:"F0122" ~snippet:"Explicit parameters");
    Test.case "snake-case-type-names ignores non-type camelCase identifiers" (fun () ->
        let source = "let userProfile = 42\n" in
        let pipeline =
          Tusk_fix.Pipeline.make
            ~rules:[ Tusk_fix.Rules.Snake_case_type_names.make () ]
            ()
        in
        let result = Tusk_fix.Pipeline.run pipeline source in
        let codes = diagnostic_codes result.diagnostics in
        Test.assert_equal ~expected:[] ~actual:codes;
        Ok ());
    Test.case "snake-case-type-names ignores module qualifiers in extensible types" (fun () ->
        let source = "type Message.t += Added\n" in
        let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.diagnostics);
        Ok ());
    Test.case "package rule override disables snake-case-type-names locally" (fun () ->
        with_tempdir "tusk_fix_config" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "kernel") in
              let package_toml = Path.(package_dir / Path.v "tusk.toml") in
              let src_dir = Path.(package_dir / Path.v "src") in
              let file = Path.(src_dir / Path.v "file.ml") in
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/kernel\"]\n\n[tusk.fix]\nrules = [\"snake-case-type-names\"]\n";
              write_file package_toml
                "[package]\nname = \"kernel\"\nversion = \"0.1.0\"\n\n[tusk.fix]\nrules = [\"-snake-case-type-names\"]\n\n[lib]\npath = \"src/kernel.ml\"\n";
              write_file file "type userProfile = int\n";
              let scope =
                Tusk_fix.Config.load_scope ~cwd:tmpdir
                |> Option.expect ~msg:"expected workspace scope"
              in
              let pipeline =
                Tusk_fix.Config.pipeline_for_file (Some scope) file
              in
              let result =
                Tusk_fix.Pipeline.run pipeline
                  ~filename:(Path.to_string file) "type userProfile = int\n"
              in
              Test.assert_equal ~expected:0
                ~actual:(List.length result.diagnostics);
              Ok ()));
    Test.case "workspace ignore patterns exclude matching files" (fun () ->
        with_tempdir "tusk_fix_ignore" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
              let src_dir = Path.(package_dir / Path.v "src") in
              let ignored = Path.(src_dir / Path.v "ignored.ml") in
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nignore = [\"ignored.ml\"]\nrules = [\"snake-case-type-names\"]\n";
              write_file Path.(package_dir / Path.v "tusk.toml")
                "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/app.ml\"\n";
              write_file ignored "type userProfile = int\n";
              let scope =
                Tusk_fix.Config.load_scope ~cwd:tmpdir
                |> Option.expect ~msg:"expected workspace scope"
              in
              Test.assert_true (Tusk_fix.Config.should_ignore_file (Some scope) ignored);
              Ok ()));
    Test.case "config shorthand enables and disables rules" (fun () ->
        with_tempdir "tusk_fix_rules" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
              let src_dir = Path.(package_dir / Path.v "src") in
              let file = Path.(src_dir / Path.v "file.ml") in
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nrules = [\"snake-case-type-names\"]\n";
              write_file Path.(package_dir / Path.v "tusk.toml")
                "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[tusk.fix]\nrules = [\"-snake-case-type-names\"]\n\n[lib]\npath = \"src/app.ml\"\n";
              write_file file "type userProfile = int\n";
              let result =
                Tusk_fix.Runner.run_files
                  ~pipeline_for_file:(Tusk_fix.Config.pipeline_for_file (Tusk_fix.Config.load_scope ~cwd:tmpdir))
                  ~mode:Tusk_fix.Runner.Check [ file ]
              in
              Test.assert_equal ~expected:0
                ~actual:result.summary.remaining_diagnostics;
              Ok ()));
    Test.case "workspace rule overrides keep builtins enabled by default" (fun () ->
        with_tempdir "tusk_fix_default_rules" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
              let src_dir = Path.(package_dir / Path.v "src") in
              let file = Path.(src_dir / Path.v "file.ml") in
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nrules = [\"-snake-case-type-names\"]\n";
              write_file Path.(package_dir / Path.v "tusk.toml")
                "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/app.ml\"\n";
              write_file file "let renderUser x = x\n";
              let result =
                Tusk_fix.Runner.run_files
                  ~pipeline_for_file:(Tusk_fix.Config.pipeline_for_file (Tusk_fix.Config.load_scope ~cwd:tmpdir))
                  ~mode:Tusk_fix.Runner.Check [ file ]
              in
              Test.assert_equal ~expected:1
                ~actual:result.summary.remaining_diagnostics;
              Ok ()));
    Test.case "config table uses explicit rule state" (fun () ->
        with_tempdir "tusk_fix_rule_state" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
              let src_dir = Path.(package_dir / Path.v "src") in
              let file = Path.(src_dir / Path.v "file.ml") in
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nrules = [{ name = \"snake-case-type-names\", state = \"enabled\" }]\n";
              write_file Path.(package_dir / Path.v "tusk.toml")
                "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[tusk.fix]\nrules = [{ name = \"snake-case-type-names\", state = \"disabled\" }]\n\n[lib]\npath = \"src/app.ml\"\n";
              write_file file "type userProfile = int\n";
              let result =
                Tusk_fix.Runner.run_files
                  ~pipeline_for_file:(Tusk_fix.Config.pipeline_for_file (Tusk_fix.Config.load_scope ~cwd:tmpdir))
                  ~mode:Tusk_fix.Runner.Check [ file ]
              in
              Test.assert_equal ~expected:0
                ~actual:result.summary.remaining_diagnostics;
              Ok ()));
    Test.case "runner apply rewrites camelCase type names" (fun () ->
        with_tempdir "tusk_fix_runner" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              write_file file "type userProfile = { name : string }\n";
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Apply file
              in
              Test.assert_true result.changed;
              Test.assert_equal ~expected:0
                ~actual:(List.length result.diagnostics);
              let actual = read_file file in
              let expected = "type user_profile = { name : string }\n" in
              Test.assert_equal ~expected ~actual;
              Ok ()));
    Test.case "check mode reports type-name issues without writing" (fun () ->
        with_tempdir "tusk_fix_check" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              let source = "type userProfile = int\n" in
              write_file file source;
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file
              in
              Test.assert_false result.changed;
              Test.assert_equal ~expected:1
                ~actual:(List.length result.diagnostics);
              Test.assert_equal ~expected:source ~actual:(read_file file);
              Ok ()));
    Test.case "check mode reports function-name issues without writing" (fun () ->
        with_tempdir "tusk_fix_check" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              let source = "let userProfile x = x\n" in
              write_file file source;
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file
              in
              Test.assert_false result.changed;
              Test.assert_equal ~expected:1
                ~actual:(List.length result.diagnostics);
              Test.assert_equal ~expected:source ~actual:(read_file file);
              Ok ()));
    Test.case "check mode reports module-name issues without writing" (fun () ->
        with_tempdir "tusk_fix_check" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              let source = "module Foo_bar = struct end\n" in
              write_file file source;
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file
              in
              Test.assert_false result.changed;
              Test.assert_equal ~expected:1
                ~actual:(List.length result.diagnostics);
              Test.assert_equal ~expected:source ~actual:(read_file file);
              Ok ()));
    Test.case "check mode reports variable-name issues without writing" (fun () ->
        with_tempdir "tusk_fix_check" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              let source = "let currentUser = 42\n" in
              write_file file source;
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file
              in
              Test.assert_false result.changed;
              Test.assert_equal ~expected:1
                ~actual:(List.length result.diagnostics);
              Test.assert_equal ~expected:source ~actual:(read_file file);
              Ok ()));
    Test.case "check mode reports prime-variable issues without writing" (fun () ->
        with_tempdir "tusk_fix_check" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              let source = "let state' = 42\n" in
              write_file file source;
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file
              in
              Test.assert_false result.changed;
              Test.assert_equal ~expected:1
                ~actual:(List.length result.diagnostics);
              Test.assert_equal ~expected:source ~actual:(read_file file);
              Ok ()));
    Test.case "check mode reports argument-name issues without writing" (fun () ->
        with_tempdir "tusk_fix_check" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              let source = "let render userId = userId\n" in
              write_file file source;
              let result =
                Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file
              in
              Test.assert_false result.changed;
              Test.assert_equal ~expected:1
                ~actual:(List.length result.diagnostics);
              Test.assert_equal ~expected:source ~actual:(read_file file);
              Ok ()));
    Test.case "cli applies safe fixes by default" (fun () ->
        with_tempdir "tusk_fix_cli" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              write_file file "type userProfile = int\n";
              let result =
                with_cwd tmpdir (fun () -> run_cli [ Path.to_string file ])
              in
              Test.assert_ok result;
              Test.assert_equal ~expected:"type user_profile = int\n"
                ~actual:(read_file file);
              Ok ()));
    Test.case "cli check exits with error when issues remain" (fun () ->
        with_tempdir "tusk_fix_cli" (fun tmpdir ->
              let file = Path.(tmpdir / Path.v "sample.ml") in
              write_file file "type userProfile = int\n";
              let result =
                with_cwd tmpdir (fun () ->
                    run_cli [ "--check"; Path.to_string file ])
              in
              Test.assert_error result;
              Test.assert_equal ~expected:"type userProfile = int\n"
                ~actual:(read_file file);
              Ok ()));
    Test.case "pipeline parses interface files with interface entrypoint" (fun () ->
        let source =
          "type ('request, 'response) t\nval create : unit -> unit\n"
        in
        let result =
          Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ())
            ~filename:"sample.mli" source
        in
        Test.assert_equal ~expected:0
          ~actual:(List.length result.parse_diagnostics);
        Ok ());
    Test.case "scanner skips syn parser corpus inputs" (fun () ->
        with_tempdir "tusk_fix_scan" (fun tmpdir ->
              let diag_dir = Path.(tmpdir / Path.v "tests" / Path.v "diagnostics") in
              let fixtures_dir = Path.(tmpdir / Path.v "tests" / Path.v "fixtures") in
              let generated_dir = Path.(tmpdir / Path.v "tests" / Path.v "generated") in
              let src_dir = Path.(tmpdir / Path.v "src") in
              Fs.create_dir_all diag_dir |> Result.expect ~msg:"mkdir diagnostics";
              Fs.create_dir_all fixtures_dir |> Result.expect ~msg:"mkdir fixtures";
              Fs.create_dir_all generated_dir |> Result.expect ~msg:"mkdir generated";
              Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
              write_file Path.(diag_dir / Path.v "bad.ml") "let =\n";
              write_file Path.(fixtures_dir / Path.v "fixture.ml") "let x = 1\n";
              write_file Path.(generated_dir / Path.v "generated.ml") "let y = 2\n";
              write_file Path.(src_dir / Path.v "real.ml") "let z = 3\n";
              let files =
                Tusk_fix.File_scanner.(scan (create ~root:tmpdir ()))
                |> List.map Path.to_string
                |> List.sort String.compare
              in
              Test.assert_equal ~expected:[ Path.to_string Path.(src_dir / Path.v "real.ml") ]
                ~actual:files;
              Ok ()));
    Test.case "config scope discovers fix providers from workspace packages" (fun () ->
        with_tempdir "tusk_fix_provider_scope" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "std") in
              Fs.create_dir_all package_dir |> Result.expect ~msg:"mkdir package";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/std\"]\n";
              write_file Path.(package_dir / Path.v "tusk.toml")
                "[package]\nname = \"std\"\nversion = \"0.1.0\"\n\n[tusk.fix.provider]\npath = \"fix/no_stdlib_provider.ml\"\nrules = [\"no-stdlib\"]\n";
              Fs.create_dir_all Path.(package_dir / Path.v "fix")
              |> Result.expect ~msg:"mkdir fix";
              write_file
                Path.(package_dir / Path.v "fix" / Path.v "no_stdlib_provider.ml")
                "let name = \"std\"\nlet rules () = []\nlet explanations () = []\n";
              let scope =
                Tusk_fix.Config.load_scope ~cwd:tmpdir
                |> Option.expect ~msg:"expected workspace scope"
              in
              match Tusk_fix.Config.providers (Some scope) with
              | [ provider ] ->
                  Test.assert_equal
                    ~expected:
                      (Path.to_string
                         Path.(
                           package_dir / Path.v "fix"
                           / Path.v "no_stdlib_provider.ml"))
                    ~actual:
                      (Path.to_string
                         provider.Tusk_model.Fix_provider.source_path);
                  Ok ()
              | _ -> Error "expected one discovered provider"));
    Test.case "config scope defaults provider path to fix/tusk_fix_rules.ml" (fun () ->
        with_tempdir "tusk_fix_provider_default_path" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
              let fix_dir = Path.(package_dir / Path.v "fix") in
              Fs.create_dir_all fix_dir |> Result.expect ~msg:"mkdir fix";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/demo\"]\n";
              write_file Path.(package_dir / Path.v "tusk.toml")
                "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n\n[tusk.fix.provider]\nrules = [\"demo-rule\"]\n";
              write_file Path.(fix_dir / Path.v "tusk_fix_rules.ml")
                "let name = \"demo\"\nlet rules () = []\nlet explanations () = []\n";
              let scope =
                Tusk_fix.Config.load_scope ~cwd:tmpdir
                |> Option.expect ~msg:"expected workspace scope"
              in
              match Tusk_fix.Config.providers (Some scope) with
              | [ provider ] ->
                  Test.assert_equal
                    ~expected:
                      (Path.to_string
                         Path.(fix_dir / Path.v "tusk_fix_rules.ml"))
                    ~actual:(Path.to_string provider.Tusk_model.Fix_provider.source_path);
                  Test.assert_equal ~expected:[ "demo:demo-rule" ] ~actual:provider.rules;
                  Ok ()
              | _ -> Error "expected one discovered provider"));
    Test.case
      "config scope defaults provider path to fix/tusk_fix_rules/tusk_fix_rules.ml"
      (fun () ->
        with_tempdir "tusk_fix_provider_nested_default_path" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
              let provider_dir =
                Path.(package_dir / Path.v "fix" / Path.v "tusk_fix_rules")
              in
              Fs.create_dir_all provider_dir |> Result.expect ~msg:"mkdir provider dir";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/demo\"]\n";
              write_file Path.(package_dir / Path.v "tusk.toml")
                "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n\n[tusk.fix.provider]\nrules = [\"demo-rule\"]\n";
              write_file Path.(provider_dir / Path.v "tusk_fix_rules.ml")
                "let name = \"demo\"\nlet rules () = []\nlet explanations () = []\n";
              let scope =
                Tusk_fix.Config.load_scope ~cwd:tmpdir
                |> Option.expect ~msg:"expected workspace scope"
              in
              match Tusk_fix.Config.providers (Some scope) with
              | [ provider ] ->
                  Test.assert_equal
                    ~expected:
                      (Path.to_string
                         Path.(
                           provider_dir / Path.v "tusk_fix_rules.ml"))
                    ~actual:(Path.to_string provider.Tusk_model.Fix_provider.source_path);
                  Test.assert_equal ~expected:[ "demo:demo-rule" ] ~actual:provider.rules;
                  Ok ()
              | _ -> Error "expected one discovered provider"));
    Test.case "fused runtime includes provider build dependencies" (fun () ->
        with_tempdir "tusk_fix_provider_build_deps" (fun tmpdir ->
              let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
              let provider_dir = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
              let helper_dir = Path.(tmpdir / Path.v "packages" / Path.v "helper") in
              let fix_dir = Path.(provider_dir / Path.v "fix") in
              let helper_src_dir = Path.(helper_dir / Path.v "src") in
              Fs.create_dir_all fix_dir |> Result.expect ~msg:"mkdir fix";
              Fs.create_dir_all helper_src_dir |> Result.expect ~msg:"mkdir helper";
              write_file workspace_toml
                "[workspace]\nmembers = [\"packages/demo\", \"packages/helper\"]\n";
              write_file Path.(provider_dir / Path.v "tusk.toml")
                "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n\n[build-dependencies]\nhelper = { path = \"../helper\" }\n\n[tusk.fix.provider]\nrules = [\"demo-rule\"]\n";
              write_file Path.(helper_dir / Path.v "tusk.toml")
                "[package]\nname = \"helper\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/helper.ml\"\n";
              write_file Path.(helper_src_dir / Path.v "helper.ml") "let value = 1\n";
              write_file Path.(fix_dir / Path.v "tusk_fix_rules.ml")
                "let name = \"demo\"\nlet rules () = []\nlet explanations () = []\n";
              let providers =
                [
                  Tusk_model.Fix_provider.
                    {
                      name = "demo";
                      package_name = "demo";
                      package_path = provider_dir;
                      source_path = Path.(fix_dir / Path.v "tusk_fix_rules.ml");
                      rules = [ "demo:demo-rule" ];
                    };
                ]
              in
              let plan =
                Tusk_fix.Fused_runtime.materialize ~workspace_root:tmpdir
                  ~target_dir_root:Path.(tmpdir / Path.v "_build")
                  providers
              in
              let package_toml = read_file plan.package_toml_path in
              Test.assert_true (String.contains package_toml "helper = { path = \"");
              Ok ()));
    Test.case "fused runtime registry source lists discovered providers" (fun () ->
        let providers =
          [
            Tusk_model.Fix_provider.
              {
                name = "std";
                package_name = "std";
                package_path = Path.v "packages/std";
                source_path = Path.v "/workspace/packages/std/fix/no_stdlib_provider.ml";
                rules = [ "std:no-stdlib" ];
              };
            Tusk_model.Fix_provider.
              {
                name = "suri";
                package_name = "suri";
                package_path = Path.v "packages/suri";
                source_path = Path.v "/workspace/packages/suri/fix/route_style_provider.ml";
                rules = [ "suri:route-style" ];
              };
          ]
        in
        let source = Tusk_fix.Fused_runtime.registry_source providers in
        Test.assert_true (String.contains source "Provider_std_std");
        Test.assert_true (String.contains source "Provider_suri_suri");
        Ok ());
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"tusk-fix:runner" ~tests ~args:Env.args)
    ~args:Env.args ()
