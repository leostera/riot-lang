(** Build actions - concrete build steps that happen in the sandbox *)

type action =
  (* File compilation actions *)
  | CompileInterface of {
      source : string;
      output : string;
      includes : string list;
    }
  | CompileImplementation of {
      source : string;
      output : string;
      includes : string list;
    }
  | CompileC of { source : string; output : string }
  (* Linking actions *)
  | CreateLibrary of {
      output : string;
      objects : string list;
      includes : string list;
    }
  | CreateExecutable of {
      output : string;
      objects : string list;
      libraries : string list;
      includes : string list;
    }
  (* File operations *)
  | CopyFile of { source : string; destination : string }
  | WriteFile of { destination : string; content : string }
  (* Output declaration *)
  | DeclareOutputs of { outputs : string list }

type action_result = Success | Failed of string | Skipped of string
type resolved_dep = { lib_file : string; include_path : string }

(* Use Hasher module for all hash operations *)

(** Convert action to canonical string for hashing *)
let action_to_string action =
  match action with
  | CompileInterface { source; output; includes } ->
      Printf.sprintf "compile_interface(%s,%s,[%s])" source output
        (String.concat "," includes)
  | CompileImplementation { source; output; includes } ->
      Printf.sprintf "compile_impl(%s,%s,[%s])" source output
        (String.concat "," includes)
  | CompileC { source; output } ->
      Printf.sprintf "compile_c(%s,%s)" source output
  | CreateLibrary { output; objects; includes } ->
      Printf.sprintf "create_library(%s,[%s],[%s])" output
        (String.concat "," objects)
        (String.concat "," includes)
  | CreateExecutable { output; objects; libraries; includes } ->
      Printf.sprintf "create_exe(%s,[%s],[%s],[%s])" output
        (String.concat "," objects)
        (String.concat "," libraries)
        (String.concat "," includes)
  | CopyFile { source; destination } ->
      Printf.sprintf "copy(%s,%s)" source destination
  | WriteFile { destination; content } ->
      Printf.sprintf "write(%s,%d bytes)" destination (String.length content)
  | DeclareOutputs { outputs } ->
      Printf.sprintf "declare_outputs([%s])" (String.concat "," outputs)

(** Pretty print an action *)
let string_of_action = function
  | CompileInterface { source; output; includes } ->
      Printf.sprintf "compile_interface(%s -> %s) [includes: %s]"
        (Filename.basename source) (Filename.basename output)
        (String.concat "; " includes)
  | CompileImplementation { source; output; includes } ->
      Printf.sprintf "compile_impl(%s -> %s) [includes: %s]"
        (Filename.basename source) (Filename.basename output)
        (String.concat "; " includes)
  | CompileC { source; output } ->
      Printf.sprintf "compile_c(%s -> %s)" (Filename.basename source)
        (Filename.basename output)
  | CreateLibrary { output; objects; includes } ->
      Printf.sprintf "create_library(%s from [%s]) [includes: %s]"
        (Filename.basename output)
        (String.concat "; " (List.map Filename.basename objects))
        (String.concat "; " includes)
  | CreateExecutable { output; objects; libraries; includes } ->
      Printf.sprintf "create_exe(%s from [%s] with [%s]) [includes: %s]"
        (Filename.basename output)
        (String.concat "; " (List.map Filename.basename objects))
        (String.concat "; " libraries)
        (String.concat "; " includes)
  | CopyFile { source; destination } ->
      Printf.sprintf "copy(%s -> %s)" (Filename.basename source)
        (Filename.basename destination)
  | WriteFile { destination; content } ->
      Printf.sprintf "write(%s, %d bytes)" destination (String.length content)
  | DeclareOutputs { outputs } ->
      Printf.sprintf "declare_outputs([%s])" (String.concat "; " outputs)

(** Execute a single build action *)
let execute_action action toolchain =
  let convert_result = function
    | Ocamlc.Success msg -> (Success, msg)
    | Ocamlc.Failed err -> (Failed err, "")
  in
  match action with
  | CompileInterface { source; output; includes } ->
      Ocamlc.compile_interface ~toolchain ~includes ~output source
      |> convert_result
  | CompileImplementation { source; output; includes } ->
      Ocamlc.compile_impl ~toolchain ~includes ~output source |> convert_result
  | CompileC { source; output } ->
      Ocamlc.compile_c ~toolchain ~includes:[] ~output source |> convert_result
  | CreateLibrary { output; objects; includes } ->
      Ocamlc.create_library ~toolchain ~includes ~output objects
      |> convert_result
  | CreateExecutable { output; objects; libraries; includes } ->
      (* Use custom executable for C stubs *)
      Ocamlc.create_custom_executable ~toolchain ~includes ~output
        ~libs:libraries objects
      |> convert_result
  | CopyFile { source; destination } -> (
      try
        let content =
          Miniriot.File.read ~path:source
          |> Result.expect ~msg:"Failed to read file"
        in
        let _ = Miniriot.File.write ~path:destination ~content in
        ();
        (Success, Printf.sprintf "Copied %s to %s" source destination)
      with exn -> (Failed (Printexc.to_string exn), ""))
  | WriteFile { destination; content } -> (
      try
        let _ = Miniriot.File.write ~path:destination ~content in
        ();
        (Success, Printf.sprintf "Wrote %s" destination)
      with exn -> (Failed (Printexc.to_string exn), ""))
  | DeclareOutputs { outputs } ->
      (* Just validate that declared outputs exist *)
      let missing =
        List.filter (fun f -> not (Miniriot.File.exists ~path:f)) outputs
      in
      if missing = [] then (Success, "All outputs exist")
      else
        ( Failed
            (Printf.sprintf "Missing outputs: %s" (String.concat ", " missing)),
          "" )

(** Hash a list of actions *)
let hash actions =
  let buffer = Buffer.create 256 in
  List.iter
    (fun action -> Buffer.add_string buffer (action_to_string action))
    actions;
  Hasher.hash_string (Buffer.contents buffer)
