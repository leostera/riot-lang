open Std

let unique_counter = Sync.Atomic.make 0

let unique_var = fun prefix ->
  let suffix = Sync.Atomic.fetch_and_add unique_counter 1 in
  prefix ^ "_" ^ Int32.to_string (Process.id ()) ^ "_" ^ Int.to_string suffix

let with_var = fun name value fn ->
  let previous = Env.set ~var:name ~value in
  let restore () =
    match previous with
    | Some old -> ignore (Env.set ~var:name ~value:old)
    | None -> ignore (Env.set ~var:name ~value:"")
  in
  match fn () with
  | Ok () as ok ->
      restore ();
      ok
  | Error _ as err ->
      restore ();
      err

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error ("failed to create temp dir: " ^ IO.error_message err)

let test_get_string_present = fun _ctx ->
  with_var
    "STD_ENV_STRING"
    "hello"
    (fun () ->
      match Env.get Env.String ~var:"STD_ENV_STRING" with
      | Some value when String.equal value "hello" -> Ok ()
      | Some _ -> Error "Env.get String returned the wrong value"
      | None -> Error "Env.get String should return Some for present variables")

let test_get_string_missing = fun _ctx ->
  let name = unique_var "STD_ENV_MISSING_STRING" in
  match Env.get Env.String ~var:name with
  | None -> Ok ()
  | Some _ -> Error "Env.get String should return None for missing variables"

let test_get_int_decimal = fun _ctx ->
  with_var
    "STD_ENV_INT"
    "123"
    (fun () ->
      match Env.get Env.Int ~var:"STD_ENV_INT" with
      | Some value when Int.equal value 123 -> Ok ()
      | Some _ -> Error "Env.get Int returned the wrong parsed value"
      | None -> Error "Env.get Int should parse decimal integers")

let test_get_int_invalid = fun _ctx ->
  with_var
    "STD_ENV_BAD_INT"
    "12x"
    (fun () ->
      match Env.get Env.Int ~var:"STD_ENV_BAD_INT" with
      | None -> Ok ()
      | Some _ -> Error "Env.get Int should reject invalid integers")

let test_get_float_decimal = fun _ctx ->
  with_var
    "STD_ENV_FLOAT"
    "3.14"
    (fun () ->
      match Env.get Env.Float ~var:"STD_ENV_FLOAT" with
      | Some value when Float.equal value 3.14 -> Ok ()
      | Some _ -> Error "Env.get Float returned the wrong parsed value"
      | None -> Error "Env.get Float should parse decimal floats")

let test_get_float_invalid = fun _ctx ->
  with_var
    "STD_ENV_BAD_FLOAT"
    "wat"
    (fun () ->
      match Env.get Env.Float ~var:"STD_ENV_BAD_FLOAT" with
      | None -> Ok ()
      | Some _ -> Error "Env.get Float should reject invalid floats")

let test_get_bool_true_false = fun _ctx ->
  with_var
    "STD_ENV_BOOL_TRUE"
    "true"
    (fun () ->
      match Env.get Env.Bool ~var:"STD_ENV_BOOL_TRUE" with
      | Some true ->
          with_var
            "STD_ENV_BOOL_FALSE"
            "false"
            (fun () ->
              match Env.get Env.Bool ~var:"STD_ENV_BOOL_FALSE" with
              | Some false -> Ok ()
              | Some true -> Error "Env.get Bool parsed false as true"
              | None -> Error "Env.get Bool rejected 'false'")
      | Some false -> Error "Env.get Bool parsed true as false"
      | None -> Error "Env.get Bool rejected 'true'")

let test_get_bool_numeric = fun _ctx ->
  with_var
    "STD_ENV_BOOL_ONE"
    "1"
    (fun () ->
      match Env.get Env.Bool ~var:"STD_ENV_BOOL_ONE" with
      | Some true ->
          with_var
            "STD_ENV_BOOL_ZERO"
            "0"
            (fun () ->
              match Env.get Env.Bool ~var:"STD_ENV_BOOL_ZERO" with
              | Some false -> Ok ()
              | _ -> Error "Env.get Bool should parse '0' as false")
      | _ -> Error "Env.get Bool should parse '1' as true")

let test_get_bool_yes_no_on_off = fun _ctx ->
  with_var
    "STD_ENV_BOOL_YES"
    "yes"
    (fun () ->
      match Env.get Env.Bool ~var:"STD_ENV_BOOL_YES" with
      | Some true ->
          with_var
            "STD_ENV_BOOL_NO"
            "no"
            (fun () ->
              match Env.get Env.Bool ~var:"STD_ENV_BOOL_NO" with
              | Some false ->
                  with_var
                    "STD_ENV_BOOL_ON"
                    "on"
                    (fun () ->
                      match Env.get Env.Bool ~var:"STD_ENV_BOOL_ON" with
                      | Some true ->
                          with_var
                            "STD_ENV_BOOL_OFF"
                            "off"
                            (fun () ->
                              match Env.get Env.Bool ~var:"STD_ENV_BOOL_OFF" with
                              | Some false -> Ok ()
                              | _ -> Error "Env.get Bool should parse 'off' as false")
                      | _ -> Error "Env.get Bool should parse 'on' as true")
              | _ -> Error "Env.get Bool should parse 'no' as false")
      | _ -> Error "Env.get Bool should parse 'yes' as true")

let test_get_bool_is_case_insensitive = fun _ctx ->
  with_var
    "STD_ENV_BOOL_MIXED"
    "TrUe"
    (fun () ->
      match Env.get Env.Bool ~var:"STD_ENV_BOOL_MIXED" with
      | Some true -> Ok ()
      | _ -> Error "Env.get Bool should parse mixed-case booleans")

let test_get_char_single_character = fun _ctx ->
  with_var
    "STD_ENV_CHAR_ONE"
    ","
    (fun () ->
      match Env.get Env.Char ~var:"STD_ENV_CHAR_ONE" with
      | Some value when Char.equal value ',' -> Ok ()
      | Some _ -> Error "Env.get Char returned the wrong character"
      | None -> Error "Env.get Char should parse single-character values")

let test_get_char_rejects_multi_character_values = fun _ctx ->
  with_var
    "STD_ENV_CHAR_MULTI"
    "ab"
    (fun () ->
      match Env.get Env.Char ~var:"STD_ENV_CHAR_MULTI" with
      | None -> Ok ()
      | Some _ -> Error "Env.get Char should reject multi-character values")

let test_set_on_previously_unset_variable = fun _ctx ->
  let name = unique_var "STD_ENV_SET_NEW" in
  let previous = Env.set ~var:name ~value:"fresh" in
  if not (Option.is_none previous) then
    Error "Env.set should return None for previously unset variables"
  else
    match Env.get Env.String ~var:name with
    | Some value when String.equal value "fresh" -> Ok ()
    | _ -> Error "Env.set should make the new value visible"

let test_set_on_previously_set_variable = fun _ctx ->
  with_var
    "STD_ENV_SET_OLD"
    "before"
    (fun () ->
      let previous = Env.set ~var:"STD_ENV_SET_OLD" ~value:"after" in
      match (previous, Env.get Env.String ~var:"STD_ENV_SET_OLD") with
      | (Some old, Some current) when String.equal old "before" && String.equal current "after" ->
          Ok ()
      | _ -> Error "Env.set should return the old value and store the new one")

let test_vars_contains_inserted_pairs = fun _ctx ->
  let name_one = unique_var "STD_ENV_VARS_ONE" in
  let name_two = unique_var "STD_ENV_VARS_TWO" in
  with_var
    name_one
    "alpha"
    (fun () ->
      with_var
        name_two
        "beta"
        (fun () ->
          let vars = Env.vars () in
          let has_one =
            List.any
              vars
              ~fn:(fun (name, value) -> String.equal name name_one && String.equal value "alpha")
          in
          let has_two =
            List.any
              vars
              ~fn:(fun (name, value) -> String.equal name name_two && String.equal value "beta")
          in
          if has_one && has_two then
            Ok ()
          else
            Error "Env.vars should contain inserted key/value pairs"))

let test_current_dir_matches_successful_set_current_dir = fun _ctx ->
  let original = Env.current_dir () in
  with_tempdir
    "std_env_current_dir"
    (fun dir ->
      match Env.set_current_dir dir with
      | Error _ -> Error "failed to set current dir"
      | Ok () ->
          let result =
            match Env.current_dir () with
            | Ok current ->
                (match (Fs.canonicalize current, Fs.canonicalize dir) with
                | (Ok current, Ok expected) when Path.equal current expected -> Ok ()
                | (Ok _, Ok _) ->
                    Error "Env.current_dir should reflect the directory set by Env.set_current_dir"
                | _ -> Error "failed to canonicalize current directory paths")
            | Error _ -> Error "Env.current_dir failed after successfully setting the current dir"
          in
          ignore
            (
              match original with
              | Ok cwd -> Env.set_current_dir cwd
              | Error _ -> Ok ()
            );
          result)

let test_set_current_dir_to_non_directory_returns_error = fun _ctx ->
  let original = Env.current_dir () in
  with_tempdir
    "std_env_non_directory"
    (fun dir ->
      let file = Path.(dir / Path.v "file.txt") in
      match Fs.write "hello" file with
      | Error err -> Error (IO.error_message err)
      | Ok () ->
          let result =
            match Env.set_current_dir file with
            | Error _ -> Ok ()
            | Ok () -> Error "Env.set_current_dir should reject non-directory paths"
          in
          ignore
            (
              match original with
              | Ok cwd -> Env.set_current_dir cwd
              | Error _ -> Ok ()
            );
          result)

let tests =
  Test.[
    case "get String returns present values" test_get_string_present;
    case "get String returns None when missing" test_get_string_missing;
    case "get Int parses decimal integers" test_get_int_decimal;
    case "get Int rejects invalid text" test_get_int_invalid;
    case "get Float parses decimal floats" test_get_float_decimal;
    case "get Float rejects invalid text" test_get_float_invalid;
    case "get Bool parses true and false" test_get_bool_true_false;
    case "get Bool parses numeric booleans" test_get_bool_numeric;
    case "get Bool parses yes no on off" test_get_bool_yes_no_on_off;
    case "get Bool is case insensitive" test_get_bool_is_case_insensitive;
    case "get Char parses single-character values" test_get_char_single_character;
    case "get Char rejects multi-character values" test_get_char_rejects_multi_character_values;
    case "set returns None for previously unset variables" test_set_on_previously_unset_variable;
    case
      "set returns the old value for previously set variables"
      test_set_on_previously_set_variable;
    case "vars contains inserted key value pairs" test_vars_contains_inserted_pairs;
    case
      "current_dir matches successful set_current_dir"
      test_current_dir_matches_successful_set_current_dir;
    case
      "set_current_dir rejects non-directory paths"
      test_set_current_dir_to_non_directory_returns_error;
  ]

let main ~args = Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"env" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
