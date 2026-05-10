open Std

let expect_int = fun ~expected ~actual ->
  if Int.equal expected actual then
    Ok ()
  else
    Error ("expected " ^ Int.to_string expected ^ ", got " ^ Int.to_string actual)

let expect_string = fun ~expected ~actual ->
  if String.equal expected actual then
    Ok ()
  else
    Error ("expected " ^ expected ^ ", got " ^ actual)

let expect_none = fun ~name actual ->
  match actual with
  | None -> Ok ()
  | Some value -> Error ("expected " ^ name ^ " to be unset, got " ^ value)

let expect_some_string = fun ~expected ~actual ->
  match actual with
  | Some actual -> expect_string ~expected ~actual
  | None -> Error ("expected " ^ expected ^ ", got <unset>")

let and_then = fun fn result -> Result.and_then result ~fn

let binding_by_key = fun key (bindings: Dotenv.binding list) ->
  let rec loop bindings =
    match bindings with
    | [] -> Error ("missing binding for " ^ key)
    | binding :: rest ->
        if String.equal Dotenv.(binding.key) key then
          Ok binding
        else
          loop rest
  in
  loop bindings

let expect_binding = fun ~key ~expected bindings ->
  binding_by_key key bindings
  |> and_then (fun binding -> expect_string ~expected ~actual:Dotenv.(binding.value))

let expect_parse_error = fun input ->
  match Dotenv.parse input with
  | Error (Dotenv.ParseError _) -> Ok ()
  | Error error -> Error (Dotenv.error_to_string error)
  | Ok _ -> Error ("expected parse failure for " ^ input)

let restore_env = fun saved ->
  let rec loop saved =
    match saved with
    | [] -> ()
    | (name, previous) :: rest ->
        (
          match previous with
          | Some value -> ignore (Env.set ~var:name ~value)
          | None -> ignore (Env.remove ~var:name)
        );
        loop rest
  in
  loop saved

let with_clean_env = fun names fn ->
  let rec save names acc =
    match names with
    | [] -> acc
    | name :: rest -> save rest ((name, Env.var Env.String ~name) :: acc)
  in
  let saved = save names [] in
  let rec clear names =
    match names with
    | [] -> ()
    | name :: rest ->
        ignore (Env.remove ~var:name);
        clear rest
  in
  clear names;
  let result = fn () in
  restore_env saved;
  result

let with_tempdir = fun fn ->
  match Fs.with_tempdir ~prefix:"dotenv_tests_" fn with
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)
  | Ok result -> result

let write_file = fun path content ->
  match Fs.write content path with
  | Ok () -> Ok ()
  | Error error -> Error ("write failed: " ^ IO.error_message error)

let vector_to_list = fun vector ->
  vector
  |> Collections.Vector.to_array
  |> Collections.Array.to_list

let expect_contains = fun ~expected values ->
  let rec loop values =
    match values with
    | [] -> Error ("missing telemetry event " ^ expected)
    | value :: rest ->
        if String.equal value expected then
          Ok ()
        else
          loop rest
  in
  loop values

let test_parse_reference_basics = fun _ctx ->
  let content =
    "\239\187\191# comment
PLAIN=value
SPACED = spaced
AFTER= value
EMPTY=
COMMENTED=kept # ignored
HASH=value#kept
SINGLE='literal # value'
DOUBLE=\"two words\"
ESCAPED=my\\ cool\\ value
YAML: 1
KEY.Value=dot
export EXPORTED=1
export=\"as-key\"
"
  in
  match Dotenv.parse content with
  | Error error -> Error (Dotenv.error_to_string error)
  | Ok bindings ->
      expect_int ~expected:13 ~actual:(List.length bindings)
      |> and_then (fun () -> expect_binding ~key:"PLAIN" ~expected:"value" bindings)
      |> and_then (fun () -> expect_binding ~key:"SPACED" ~expected:"spaced" bindings)
      |> and_then (fun () -> expect_binding ~key:"AFTER" ~expected:"value" bindings)
      |> and_then (fun () -> expect_binding ~key:"EMPTY" ~expected:"" bindings)
      |> and_then (fun () -> expect_binding ~key:"COMMENTED" ~expected:"kept" bindings)
      |> and_then (fun () -> expect_binding ~key:"HASH" ~expected:"value#kept" bindings)
      |> and_then (fun () -> expect_binding ~key:"SINGLE" ~expected:"literal # value" bindings)
      |> and_then (fun () -> expect_binding ~key:"DOUBLE" ~expected:"two words" bindings)
      |> and_then (fun () -> expect_binding ~key:"ESCAPED" ~expected:"my cool value" bindings)
      |> and_then (fun () -> expect_binding ~key:"YAML" ~expected:"1" bindings)
      |> and_then (fun () -> expect_binding ~key:"KEY.Value" ~expected:"dot" bindings)
      |> and_then (fun () -> expect_binding ~key:"EXPORTED" ~expected:"1" bindings)
      |> and_then (fun () -> expect_binding ~key:"export" ~expected:"as-key" bindings)

let test_parse_quoted_escapes = fun _ctx ->
  let content =
    "ESCAPED=\"line\\nnext\\rreturn\\ttab\"
QUOTE=\"awesome \\\"stuff\\\"\"
NO_SUB=\"\\$VAR \\${VAR}\"
SINGLE='sweet ${VAR} $VAR'
"
  in
  match Dotenv.parse content with
  | Error error -> Error (Dotenv.error_to_string error)
  | Ok bindings ->
      expect_binding ~key:"ESCAPED" ~expected:"line\nnext\rreturn\ttab" bindings
      |> and_then (fun () -> expect_binding ~key:"QUOTE" ~expected:"awesome \"stuff\"" bindings)
      |> and_then (fun () -> expect_binding ~key:"NO_SUB" ~expected:"$VAR ${VAR}" bindings)
      |> and_then (fun () -> expect_binding ~key:"SINGLE" ~expected:"sweet ${VAR} $VAR" bindings)

let test_parse_variable_substitution = fun _ctx ->
  with_clean_env
    [ "BERRY_DOTENV_SUB_ENV" ]
    (fun () ->
      ignore (Env.set ~var:"BERRY_DOTENV_SUB_ENV" ~value:"from-env");
      let content =
        "BERRY_DOTENV_LOCAL=from-file
FROM_LOCAL=$BERRY_DOTENV_LOCAL
FROM_BRACES=${BERRY_DOTENV_LOCAL}-suffix
FROM_ENV=${BERRY_DOTENV_SUB_ENV}
MISSING=before-${BERRY_DOTENV_MISSING}-after
DOUBLE=\"quote $BERRY_DOTENV_LOCAL ${BERRY_DOTENV_SUB_ENV}\"
SINGLE='quote $BERRY_DOTENV_LOCAL'
ESCAPED=\\$BERRY_DOTENV_LOCAL
"
      in
      match Dotenv.parse content with
      | Error error -> Error (Dotenv.error_to_string error)
      | Ok bindings ->
          expect_binding ~key:"FROM_LOCAL" ~expected:"from-file" bindings
          |> and_then
            (fun () ->
              expect_binding ~key:"FROM_BRACES" ~expected:"from-file-suffix" bindings)
          |> and_then (fun () -> expect_binding ~key:"FROM_ENV" ~expected:"from-env" bindings)
          |> and_then (fun () -> expect_binding ~key:"MISSING" ~expected:"before--after" bindings)
          |> and_then
            (fun () ->
              expect_binding ~key:"DOUBLE" ~expected:"quote from-file from-env" bindings)
          |> and_then
            (fun () ->
              expect_binding ~key:"SINGLE" ~expected:"quote $BERRY_DOTENV_LOCAL" bindings)
          |> and_then
            (fun () ->
              expect_binding ~key:"ESCAPED" ~expected:"$BERRY_DOTENV_LOCAL" bindings))

let test_parse_environment_overrides_substitution = fun _ctx ->
  with_clean_env
    [ "BERRY_DOTENV_OVERRIDE_SOURCE" ]
    (fun () ->
      ignore (Env.set ~var:"BERRY_DOTENV_OVERRIDE_SOURCE" ~value:"shell");
      match Dotenv.parse
        "BERRY_DOTENV_OVERRIDE_SOURCE=file
DERIVED=${BERRY_DOTENV_OVERRIDE_SOURCE}
" with
      | Error error -> Error (Dotenv.error_to_string error)
      | Ok bindings -> expect_binding ~key:"DERIVED" ~expected:"shell" bindings)

let test_parse_substitution_boundaries = fun _ctx ->
  with_clean_env
    [ "BERRY_DOTENV_HOST"; "BERRY_DOTENV_KEY.Value"; "BERRY_DOTENV_KEY1"; "BERRY_DOTENV_KEY1_1"; ]
    (fun () ->
      let content =
        "BERRY_DOTENV_HOST=example
URL=https://$BERRY_DOTENV_HOST.example.com
BERRY_DOTENV_KEY.Value=dotted
FROM_DOTTED=${BERRY_DOTENV_KEY.Value}
BERRY_DOTENV_KEY1=test_user
BERRY_DOTENV_KEY1_1=test_user_with_separator
FULL_UNBRACED=$BERRY_DOTENV_KEY1_1
BRACED_SUFFIX=${BERRY_DOTENV_KEY1}_1
DOLLAR=\"bar $ \"
ESCAPED_DOLLAR=\"bar\\$ \\$\\$\"
"
      in
      match Dotenv.parse content with
      | Error error -> Error (Dotenv.error_to_string error)
      | Ok bindings ->
          expect_binding ~key:"URL" ~expected:"https://example.example.com" bindings
          |> and_then (fun () -> expect_binding ~key:"FROM_DOTTED" ~expected:"dotted" bindings)
          |> and_then
            (fun () ->
              expect_binding
                ~key:"FULL_UNBRACED"
                ~expected:"test_user_with_separator"
                bindings)
          |> and_then
            (fun () ->
              expect_binding ~key:"BRACED_SUFFIX" ~expected:"test_user_1" bindings)
          |> and_then (fun () -> expect_binding ~key:"DOLLAR" ~expected:"bar $ " bindings)
          |> and_then (fun () -> expect_binding ~key:"ESCAPED_DOLLAR" ~expected:"bar$ $$" bindings))

let test_parse_multiline_values = fun _ctx ->
  let content =
    "SINGLE='line 1
line 2
line 3'
DOUBLE=\"line 1
line 2 \\\"quoted\\\"
line 3\"
AFTER=done
"
  in
  match Dotenv.parse content with
  | Error error -> Error (Dotenv.error_to_string error)
  | Ok bindings ->
      expect_binding ~key:"SINGLE" ~expected:"line 1\nline 2\nline 3" bindings
      |> and_then
        (fun () ->
          expect_binding
            ~key:"DOUBLE"
            ~expected:"line 1\nline 2 \"quoted\"\nline 3"
            bindings)
      |> and_then (fun () -> expect_binding ~key:"AFTER" ~expected:"done" bindings)

let test_parse_crlf_and_cr = fun _ctx ->
  match Dotenv.parse "A=1\r\nB=2\rC=3" with
  | Error error -> Error (Dotenv.error_to_string error)
  | Ok bindings ->
      expect_binding ~key:"A" ~expected:"1" bindings
      |> and_then (fun () -> expect_binding ~key:"B" ~expected:"2" bindings)
      |> and_then (fun () -> expect_binding ~key:"C" ~expected:"3" bindings)

let test_export_existing_without_value = fun _ctx ->
  match Dotenv.parse "OPTION_A=2\nexport OPTION_A\nOPTION_B=3" with
  | Error error -> Error (Dotenv.error_to_string error)
  | Ok bindings ->
      expect_int ~expected:2 ~actual:(List.length bindings)
      |> and_then (fun () -> expect_binding ~key:"OPTION_A" ~expected:"2" bindings)
      |> and_then (fun () -> expect_binding ~key:"OPTION_B" ~expected:"3" bindings)

let test_parse_rejects_invalid_inputs = fun _ctx ->
  expect_parse_error "1BAD=value"
  |> and_then (fun () -> expect_parse_error ".BAD=value")
  |> and_then (fun () -> expect_parse_error "KEY")
  |> and_then (fun () -> expect_parse_error "KEY=\"unterminated")
  |> and_then (fun () -> expect_parse_error "KEY='unterminated")
  |> and_then (fun () -> expect_parse_error "KEY=bad\\f")
  |> and_then (fun () -> expect_parse_error "export NOT_SET")
  |> and_then (fun () -> expect_parse_error "KEY=\"ok\" trailing")
  |> and_then (fun () -> expect_parse_error "KEY=${}")
  |> and_then (fun () -> expect_parse_error "KEY=${.BAD}")

let test_load_string_preserves_existing_env = fun _ctx ->
  with_clean_env
    [ "BERRY_DOTENV_KEEP"; "BERRY_DOTENV_NEW" ]
    (fun () ->
      ignore (Env.set ~var:"BERRY_DOTENV_KEEP" ~value:"shell");
      match Dotenv.load_string "BERRY_DOTENV_KEEP=file\nBERRY_DOTENV_NEW=value" with
      | Error error -> Error (Dotenv.error_to_string error)
      | Ok applied ->
          expect_some_string
            ~expected:"shell"
            ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_KEEP")
          |> and_then
            (fun () ->
              expect_some_string
                ~expected:"value"
                ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_NEW"))
          |> and_then (fun () -> expect_int ~expected:1 ~actual:(List.length applied))
          |> and_then (fun () -> expect_binding ~key:"BERRY_DOTENV_NEW" ~expected:"value" applied))

let test_load_string_can_overwrite_existing = fun _ctx ->
  with_clean_env
    [ "BERRY_DOTENV_OVERWRITE" ]
    (fun () ->
      ignore (Env.set ~var:"BERRY_DOTENV_OVERWRITE" ~value:"shell");
      match Dotenv.load_string ~on_existing:Dotenv.OverwriteExisting "BERRY_DOTENV_OVERWRITE=file" with
      | Error error -> Error (Dotenv.error_to_string error)
      | Ok applied ->
          expect_some_string
            ~expected:"file"
            ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_OVERWRITE")
          |> and_then (fun () -> expect_int ~expected:1 ~actual:(List.length applied)))

let test_load_env_profile_layers = fun _ctx ->
  with_clean_env
    [ "BERRY_DOTENV_LAYER_SHARED"; "BERRY_DOTENV_LAYER_BASE"; "BERRY_DOTENV_LAYER_TEST"; ]
    (fun () ->
      with_tempdir
        (fun dir ->
          let base = Path.join dir (Path.v ".env") in
          let profile = Path.join dir (Path.v ".env.test") in
          write_file base "BERRY_DOTENV_LAYER_SHARED=base
BERRY_DOTENV_LAYER_BASE=base
"
          |> and_then
            (fun () ->
              write_file
                profile
                "BERRY_DOTENV_LAYER_SHARED=test
BERRY_DOTENV_LAYER_TEST=test
")
          |> and_then
            (fun () ->
              match Dotenv.load ~path:base ~env:"test" () with
              | Error error -> Error (Dotenv.error_to_string error)
              | Ok applied ->
                  expect_int ~expected:3 ~actual:(List.length applied)
                  |> and_then
                    (fun () ->
                      expect_some_string
                        ~expected:"test"
                        ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_LAYER_SHARED"))
                  |> and_then
                    (fun () ->
                      expect_some_string
                        ~expected:"base"
                        ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_LAYER_BASE"))
                  |> and_then
                    (fun () ->
                      expect_some_string
                        ~expected:"test"
                        ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_LAYER_TEST")))))

let test_load_env_profile_overwrite_keeps_profile_precedence = fun _ctx ->
  with_clean_env
    [ "BERRY_DOTENV_OVERWRITE_SHARED"; "BERRY_DOTENV_OVERWRITE_BASE" ]
    (fun () ->
      ignore (Env.set ~var:"BERRY_DOTENV_OVERWRITE_SHARED" ~value:"shell");
      with_tempdir
        (fun dir ->
          let base = Path.join dir (Path.v ".env") in
          let profile = Path.join dir (Path.v ".env.test") in
          write_file base "BERRY_DOTENV_OVERWRITE_SHARED=base
BERRY_DOTENV_OVERWRITE_BASE=base
"
          |> and_then (fun () -> write_file profile "BERRY_DOTENV_OVERWRITE_SHARED=test\n")
          |> and_then
            (fun () ->
              match Dotenv.load ~path:base ~env:"test" ~on_existing:Dotenv.OverwriteExisting () with
              | Error error -> Error (Dotenv.error_to_string error)
              | Ok applied ->
                  expect_int ~expected:3 ~actual:(List.length applied)
                  |> and_then
                    (fun () ->
                      expect_some_string
                        ~expected:"test"
                        ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_OVERWRITE_SHARED"))
                  |> and_then
                    (fun () ->
                      expect_some_string
                        ~expected:"base"
                        ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_OVERWRITE_BASE")))))

let test_load_files_overwrite_preserves_first_file_precedence = fun _ctx ->
  with_clean_env
    [ "BERRY_DOTENV_IMPORTANT_SHARED"; "BERRY_DOTENV_IMPORTANT_ONLY"; "BERRY_DOTENV_PLAIN_ONLY"; ]
    (fun () ->
      with_tempdir
        (fun dir ->
          let important = Path.join dir (Path.v "important.env") in
          let plain = Path.join dir (Path.v "plain.env") in
          write_file
            important
            "BERRY_DOTENV_IMPORTANT_SHARED=important
BERRY_DOTENV_IMPORTANT_ONLY=yes
"
          |> and_then
            (fun () ->
              write_file
                plain
                "BERRY_DOTENV_IMPORTANT_SHARED=plain
BERRY_DOTENV_PLAIN_ONLY=yes
")
          |> and_then
            (fun () ->
              match Dotenv.load_files ~on_existing:Dotenv.OverwriteExisting [ important; plain ] with
              | Error error -> Error (Dotenv.error_to_string error)
              | Ok applied ->
                  expect_int ~expected:4 ~actual:(List.length applied)
                  |> and_then
                    (fun () ->
                      expect_some_string
                        ~expected:"important"
                        ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_IMPORTANT_SHARED"))
                  |> and_then
                    (fun () ->
                      expect_some_string
                        ~expected:"yes"
                        ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_IMPORTANT_ONLY"))
                  |> and_then
                    (fun () ->
                      expect_some_string
                        ~expected:"yes"
                        ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_PLAIN_ONLY")))))

let test_load_if_exists_allows_profile_without_base = fun _ctx ->
  with_clean_env
    [ "BERRY_DOTENV_LOCAL_ONLY" ]
    (fun () ->
      with_tempdir
        (fun dir ->
          let base = Path.join dir (Path.v ".env") in
          let profile = Path.join dir (Path.v ".env.local") in
          write_file profile "BERRY_DOTENV_LOCAL_ONLY=yes\n"
          |> and_then
            (fun () ->
              match Dotenv.load_if_exists ~path:base ~env:"local" () with
              | Error error -> Error (Dotenv.error_to_string error)
              | Ok applied ->
                  expect_int ~expected:1 ~actual:(List.length applied)
                  |> and_then
                    (fun () ->
                      expect_some_string
                        ~expected:"yes"
                        ~actual:(Env.var Env.String ~name:"BERRY_DOTENV_LOCAL_ONLY")))))

let test_load_if_exists_skips_missing = fun _ctx ->
  with_tempdir
    (fun dir ->
      let path = Path.join dir (Path.v ".env") in
      match Dotenv.load_if_exists ~path () with
      | Error error -> Error (Dotenv.error_to_string error)
      | Ok bindings -> expect_int ~expected:0 ~actual:(List.length bindings))

let test_load_missing_errors = fun _ctx ->
  with_tempdir
    (fun dir ->
      let path = Path.join dir (Path.v ".env.missing") in
      match Dotenv.load ~path () with
      | Error (Dotenv.ReadError _) -> Ok ()
      | Error error -> Error (Dotenv.error_to_string error)
      | Ok _ -> Error "expected missing file read error")

let test_parse_files_does_not_modify_env = fun _ctx ->
  with_clean_env
    [ "BERRY_DOTENV_PARSE_ONLY" ]
    (fun () ->
      with_tempdir
        (fun dir ->
          let path = Path.join dir (Path.v "plain.env") in
          write_file path "BERRY_DOTENV_PARSE_ONLY=value\n"
          |> and_then
            (fun () ->
              match Dotenv.parse_files [ path ] with
              | Error error -> Error (Dotenv.error_to_string error)
              | Ok bindings ->
                  expect_binding ~key:"BERRY_DOTENV_PARSE_ONLY" ~expected:"value" bindings
                  |> and_then
                    (fun () ->
                      expect_none
                        ~name:"BERRY_DOTENV_PARSE_ONLY"
                        (Env.var Env.String ~name:"BERRY_DOTENV_PARSE_ONLY")))))

let test_env_paths = fun _ctx ->
  match Dotenv.env_paths ~env:"dev" () with
  | [ first; second ] ->
      expect_string ~expected:".env.dev" ~actual:(Path.to_string first)
      |> and_then (fun () -> expect_string ~expected:".env" ~actual:(Path.to_string second))
  | _ -> Error "expected .env.dev and .env"

let test_parse_and_load_emit_telemetry = fun _ctx ->
  ignore (Telemetry.start ());
  let observed = Collections.Vector.create () in
  Telemetry.attach
    "dotenv-hardening-tests"
    (fun event ->
      match event with
      | Dotenv.Events.Parsed { binding_count } ->
          Collections.Vector.push observed ~value:("parsed:" ^ Int.to_string binding_count)
      | Dotenv.Events.Loaded { path; binding_count } ->
          Collections.Vector.push
            observed
            ~value:("loaded:" ^ Path.to_string path ^ ":" ^ Int.to_string binding_count)
      | Dotenv.Events.LoadSkipped { path } ->
          Collections.Vector.push observed ~value:("skipped:" ^ Path.to_string path)
      | _ -> ());
  let result =
    with_tempdir
      (fun dir ->
        let base = Path.join dir (Path.v ".env") in
        let missing = Path.join dir (Path.v ".env.missing") in
        write_file base "BERRY_DOTENV_TELEMETRY=1\n"
        |> and_then
          (fun () ->
            ignore (Dotenv.parse "A=1\nB=2");
            match Dotenv.load_if_exists ~path:missing () with
            | Error error -> Error (Dotenv.error_to_string error)
            | Ok _ -> (
                match Dotenv.load_if_exists ~path:base () with
                | Error error -> Error (Dotenv.error_to_string error)
                | Ok _ ->
                    Telemetry.stop ();
                    let values = vector_to_list observed in
                    expect_contains ~expected:"parsed:2" values
                    |> and_then
                      (fun () ->
                        expect_contains
                          ~expected:("skipped:" ^ Path.to_string missing)
                          values)
                    |> and_then
                      (fun () ->
                        expect_contains
                          ~expected:("loaded:" ^ Path.to_string base ^ ":1")
                          values)
              )))
  in
  Telemetry.detach "dotenv-hardening-tests";
  result

let tests =
  Test.[
    case "parse reference basics" test_parse_reference_basics;
    case "parse quoted escapes" test_parse_quoted_escapes;
    case "parse variable substitution" test_parse_variable_substitution;
    case "parse environment overrides substitution" test_parse_environment_overrides_substitution;
    case "parse substitution boundaries" test_parse_substitution_boundaries;
    case "parse multiline values" test_parse_multiline_values;
    case "parse crlf and cr" test_parse_crlf_and_cr;
    case "export existing without value" test_export_existing_without_value;
    case "parse rejects invalid inputs" test_parse_rejects_invalid_inputs;
    case "load string preserves existing env" test_load_string_preserves_existing_env;
    case "load string can overwrite existing" test_load_string_can_overwrite_existing;
    case "load env profile layers" test_load_env_profile_layers;
    case
      "load env profile overwrite keeps profile precedence"
      test_load_env_profile_overwrite_keeps_profile_precedence;
    case
      "load files overwrite preserves first file precedence"
      test_load_files_overwrite_preserves_first_file_precedence;
    case
      "load if exists allows profile without base"
      test_load_if_exists_allows_profile_without_base;
    case "load if exists skips missing" test_load_if_exists_skips_missing;
    case "load missing errors" test_load_missing_errors;
    case "parse files does not modify env" test_parse_files_does_not_modify_env;
    case "env paths" test_env_paths;
    case "parse and load emit telemetry" test_parse_and_load_emit_telemetry;
  ]

let main ~args = Test.Cli.main ~name:"dotenv_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
