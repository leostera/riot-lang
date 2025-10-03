open Std
open Model
open Ocaml

(** Build actions - concrete build steps that happen in the sandbox *)

type action =
  (* File compilation actions *)
  | CompileInterface of {
      source : Path.t;
      output : Path.t;
      includes : Path.t list;
      flags : Ocamlc.compiler_flag list;
    }
  | CompileImplementation of {
      source : Path.t;
      output : Path.t;
      includes : Path.t list;
      flags : Ocamlc.compiler_flag list;
    }
  | GenerateInterface of {
      source : Path.t;
      output : Path.t;
      includes : Path.t list;
      flags : Ocamlc.compiler_flag list;
    }
  | CompileC of { source : Path.t; output : Path.t }
  (* Linking actions *)
  | CreateLibrary of {
      output : Path.t;
      objects : Path.t list;
      includes : Path.t list;
    }
  | CreateExecutable of {
      output : Path.t;
      objects : Path.t list;
      libraries : Path.t list;
      includes : Path.t list;
    }
  (* File operations *)
  | CopyDir of { source : Path.t; destination : Path.t }
  | CopyFile of { source : Path.t; destination : Path.t }
  | WriteFile of { destination : Path.t; content : string }
  (* Output declaration *)
  | DeclareOutputs of { outputs : Path.t list }

type action_result = Success | Failed of string | Skipped of string
type resolved_dep = { lib_file : string; include_path : string }

(* Use Hasher module for all hash operations *)

(** Convert action to canonical string for hashing *)
let action_to_string action =
  match action with
  | CompileInterface { source; output; includes; flags } ->
      let flags_str =
        flags
        |> List.map (function
          | Ocamlc.NoAliasDeps -> "no-alias-deps"
          | Ocamlc.Open m -> "open:" ^ m
          | Ocamlc.NoStdlib -> "nostdlib"
          | Ocamlc.NoPervasives -> "nopervasives"
          | Ocamlc.Impl file -> "impl:" ^ Std.Path.to_string file
          | Ocamlc.Warning warnings ->
              let warning_strs =
                List.map
                  (function
                    | Ocamlc.NoCmiFile -> "no-cmi-file" | Ocamlc.All -> "all")
                  warnings
              in
              "warning:[" ^ String.concat ";" warning_strs ^ "]")
        |> String.concat ","
      in
      format "compile_interface(%s,%s,[%s],[%s])" (Path.to_string source)
        (Path.to_string output)
        (String.concat "," (List.map Path.to_string includes))
        flags_str
  | CompileImplementation { source; output; includes; flags } ->
      let flags_str =
        flags
        |> List.map (function
          | Ocamlc.NoAliasDeps -> "no-alias-deps"
          | Ocamlc.Open m -> "open:" ^ m
          | Ocamlc.NoStdlib -> "nostdlib"
          | Ocamlc.NoPervasives -> "nopervasives"
          | Ocamlc.Impl file -> "impl:" ^ Std.Path.to_string file
          | Ocamlc.Warning warnings ->
              let warning_strs =
                List.map
                  (function
                    | Ocamlc.NoCmiFile -> "no-cmi-file" | Ocamlc.All -> "all")
                  warnings
              in
              "warning:[" ^ String.concat ";" warning_strs ^ "]")
        |> String.concat ","
      in
      format "compile_impl(%s,%s,[%s],[%s])" (Path.to_string source)
        (Path.to_string output)
        (String.concat "," (List.map Path.to_string includes))
        flags_str
  | GenerateInterface { source; output; includes; flags } ->
      let flags_str =
        flags
        |> List.map (function
          | Ocamlc.NoAliasDeps -> "no-alias-deps"
          | Ocamlc.Open m -> "open:" ^ m
          | Ocamlc.NoStdlib -> "nostdlib"
          | Ocamlc.NoPervasives -> "nopervasives"
          | Ocamlc.Impl file -> "impl:" ^ Std.Path.to_string file
          | Ocamlc.Warning warnings ->
              let warning_strs =
                List.map
                  (function
                    | Ocamlc.NoCmiFile -> "no-cmi-file" | Ocamlc.All -> "all")
                  warnings
              in
              "warning:[" ^ String.concat ";" warning_strs ^ "]")
        |> String.concat ","
      in
      format "generate_interface(%s -> %s)[includes: %s][%s])"
        (Path.to_string source) (Path.to_string output)
        (String.concat "," (List.map Path.to_string includes))
        flags_str
  | CompileC { source; output } ->
      format "compile_c(%s,%s)" (Path.to_string source) (Path.to_string output)
  | CreateLibrary { output; objects; includes } ->
      format "create_library(%s,[%s],[%s])" (Path.to_string output)
        (String.concat "," (List.map Path.to_string objects))
        (String.concat "," (List.map Path.to_string includes))
  | CreateExecutable { output; objects; libraries; includes } ->
      format "create_exe(%s,[%s],[%s],[%s])" (Path.to_string output)
        (String.concat "," (List.map Path.to_string objects))
        (String.concat "," (List.map Path.to_string libraries))
        (String.concat "," (List.map Path.to_string includes))
  | CopyDir { source; destination } ->
      format "copydir(%s,%s)" (Path.to_string source)
        (Path.to_string destination)
  | CopyFile { source; destination } ->
      format "copy(%s,%s)" (Path.to_string source) (Path.to_string destination)
  | WriteFile { destination; content } ->
      format "write(%s,%d bytes)"
        (Path.to_string destination)
        (String.length content)
  | DeclareOutputs { outputs } ->
      format "declare_outputs([%s])"
        (String.concat "," (List.map Path.to_string outputs))

(** Pretty print an action *)
let string_of_action = function
  | CompileInterface { source; output; includes } ->
      format "compile_interface(%s -> %s) [includes: %s]" (Path.basename source)
        (Path.basename output)
        (String.concat "; " (List.map Path.to_string includes))
  | CompileImplementation { source; output; includes } ->
      format "compile_impl(%s -> %s) [includes: %s]" (Path.basename source)
        (Path.basename output)
        (String.concat "; " (List.map Path.to_string includes))
  | GenerateInterface { source; output; includes } ->
      format "generate_interface(%s -> %s) [includes: %s]"
        (Path.basename source) (Path.basename output)
        (String.concat "; " (List.map Path.to_string includes))
  | CompileC { source; output } ->
      format "compile_c(%s -> %s)" (Path.basename source) (Path.basename output)
  | CreateLibrary { output; objects; includes } ->
      format "create_library(%s from [%s]) [includes: %s]"
        (Path.basename output)
        (String.concat "; " (List.map Path.basename objects))
        (String.concat "; " (List.map Path.to_string includes))
  | CreateExecutable { output; objects; libraries; includes } ->
      format "create_exe(%s from [%s] with [%s]) [includes: %s]"
        (Path.basename output)
        (String.concat "; " (List.map Path.basename objects))
        (String.concat "; " (List.map Path.to_string libraries))
        (String.concat "; " (List.map Path.to_string includes))
  | CopyDir { source; destination } ->
      format "copydir(%s -> %s)" (Path.to_string source)
        (Path.to_string destination)
  | CopyFile { source; destination } ->
      format "copy(%s -> %s)" (Path.basename source) (Path.basename destination)
  | WriteFile { destination; content } ->
      format "write(%s, %d bytes)"
        (Path.to_string destination)
        (String.length content)
  | DeclareOutputs { outputs } ->
      format "declare_outputs([%s])"
        (String.concat "; " (List.map Path.to_string outputs))

(** Execute a single build action *)
let execute_action action toolchain cwd =
  let convert_result = function
    | Ocamlc.Success msg -> (Success, msg)
    | Ocamlc.Failed err -> (Failed err, "")
  in
  (* Execute action with relative paths *)
  (* Note: We no longer change the global cwd here - Ocamlc functions handle cwd internally *)
  let result =
    try
      match action with
      | CompileInterface { source; output; includes; flags } ->
          Ocamlc.compile_interface ~toolchain ~cwd ~includes ~flags ~output
            source
          |> convert_result
      | CompileImplementation { source; output; includes; flags } ->
          Ocamlc.compile_impl ~toolchain ~cwd ~includes ~flags ~output source
          |> convert_result
      | GenerateInterface { source; output; includes; flags } ->
          Ocamlc.generate_interface ~toolchain ~cwd ~includes ~flags ~output
            source
          |> convert_result
      | CompileC { source; output } ->
          Ocamlc.compile_c ~toolchain ~cwd ~includes:[] ~output source
          |> convert_result
      | CreateLibrary { output; objects; includes } ->
          Ocamlc.create_library ~toolchain ~cwd ~includes ~output objects
          |> convert_result
      | CreateExecutable { output; objects; libraries; includes } ->
          (* Use custom executable for C stubs *)
          Ocamlc.create_custom_executable ~toolchain ~cwd ~includes ~output
            ~libs:libraries objects
          |> convert_result
      | CopyDir { source; destination } -> (
          try
            (* Use cp -r to recursively copy directory *)
            let cmd =
              Command.make
                ~args:
                  [ "-r"; Path.to_string source; Path.to_string destination ]
                "cp"
            in
            let result = Command.status cmd in
            match result with
            | Ok 0 ->
                ( Success,
                  format "Copied directory %s to %s" (Path.to_string source)
                    (Path.to_string destination) )
            | Ok exit_code ->
                ( Failed (format "cp command failed with exit code %d" exit_code),
                  "" )
            | Error (Command.SystemError err) ->
                (Failed (format "cp command failed: %s" err), "")
          with exn -> (Failed (Exception.to_string exn), ""))
      | CopyFile { source; destination } -> (
          try
            (* Create parent directories for destination *)
            let parent_dir = Path.dirname destination in
            let _ =
              Fs.create_dir_all parent_dir
              |> Result.expect ~msg:"Failed to create parent directories"
            in
            (* Copy the file *)
            let _ =
              Fs.copy ~src:source ~dst:destination
              |> Result.expect ~msg:"Failed to copy file"
            in
            ( Success,
              format "Copied %s to %s" (Path.to_string source)
                (Path.to_string destination) )
          with exn -> (Failed (Exception.to_string exn), ""))
      | WriteFile { destination; content } -> (
          try
            let destination = Path.(cwd / destination) in
            (* Create parent directories for destination *)
            let parent_dir = Path.dirname destination in
            Fs.create_dir_all parent_dir
            |> Result.expect
                 ~msg:
                   (format "Failed to create parent directories for: %s"
                      (Path.to_string destination));
            (* Write the file *)
            let _ =
              match Fs.write content destination with
              | Ok _ -> ()
              | Error Fs.(SystemError reason) ->
                  panic
                    (format "Failed to write file: %s due to %s"
                       (Path.to_string destination)
                       reason)
            in
            (Success, format "Wrote %s" (Path.to_string destination))
          with exn -> (Failed (Exception.to_string exn), ""))
      | DeclareOutputs { outputs } ->
          (* Just record the expected outputs - don't validate yet as they haven't been built *)
          (Success, format "Declared %d expected outputs" (List.length outputs))
    with exn -> raise exn
  in
  result

(** Hash a list of actions *)
let hash actions =
  let buffer = Buffer.create 256 in
  List.iter
    (fun action -> Buffer.add_string buffer (action_to_string action))
    actions;
  Std.Crypto.hash_string (Buffer.contents buffer)

let hash_action (type state)
    (module H : Std.Crypto.Hasher.Intf with type state = state) (hasher : state)
    (action : action) =
  H.write_string hasher (action_to_string action)

let hash_actions (type state)
    (module H : Std.Crypto.Hasher.Intf with type state = state) (hasher : state)
    (actions : action list) =
  List.iter (hash_action (module H) hasher) actions
