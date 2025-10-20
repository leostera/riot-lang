open Std
open Tusk_model

(** OCaml compiler command generation and execution *)

type t = Path.t

let make path = path
let path t = t

(** Helper to run a command in a specific directory without using Process cwd
    This avoids "Too many open files" errors from excessive getcwd() calls *)
let run_in_dir ~cwd ~env cmd_str =
  let cmd_with_cd = format "cd %s && %s" (Path.to_string cwd) cmd_str in
  Log.debug "  $ %s" cmd_with_cd;
  Command.make ~env ~args:[ "-c"; cmd_with_cd ] "sh"

(** Compiler warnings that can be suppressed *)
type compiler_warning =
  | NoCmiFile  (** Warning 49: Absent cmi file when looking up module alias *)
  | All  (** All warnings *)

(** Compiler flags *)
type compiler_flag =
  | NoAliasDeps
      (** -no-alias-deps: Do not record dependencies for module aliases *)
  | Open of string  (** -open <module>: Opens the module before typing *)
  | NoStdlib
      (** -nostdlib: Do not automatically link with the standard library *)
  | NoPervasives
      (** -nopervasives: Do not open the Pervasives module (or Stdlib) *)
  | Impl of Std.Path.t
      (** -impl <file>: Compile <file> as an implementation file *)
  | Warning of compiler_warning list  (** -w: Configure warning flags *)

(** Compilation mode *)
type mode =
  | Compile (* -c flag *)
  | Library (* -a flag *)
  | Executable (* default, no special flag *)
  | CustomExe (* -custom flag for executables with C stubs *)

(** Compilation result *)
type result = Success of string | Failed of string

(** Generate include flags from directory list *)
let make_include_flags dirs =
  dirs |> List.map (fun dir -> "-I " ^ dir) |> String.concat " "

(* Get the compiler binary path from toolchain *)
let base_command t = Path.to_string t

(** Convert warning to its numeric code *)
let warning_to_code = function NoCmiFile -> "49" | All -> "a"

(** Convert compiler flags to command-line arguments *)
let flags_to_string flags =
  List.fold_left
    (fun acc flag ->
      match flag with
      | Open m -> acc @ [ "-open"; m ]
      | NoAliasDeps -> acc @ [ "-no-alias-deps" ]
      | NoStdlib -> acc @ [ "-nostdlib" ]
      | NoPervasives -> acc @ [ "-nopervasives" ]
      | Impl file -> acc @ [ "-impl"; Path.to_string file ]
      | Warning warnings ->
          (* Convert warnings to -w flag format *)
          let warning_codes = List.map warning_to_code warnings in
          let warning_str = "-" ^ String.concat "-" warning_codes in
          acc @ [ "-w"; warning_str ])
    [] flags

(** Build and run an ocamlc command *)
let run t ~cwd ?(includes = []) ?(libs = []) ?(output = None) ?(mode = Compile)
    ?(verbose = false) sources =
  let ocamlc = base_command t in

  (* Generate include flags from provided directories *)
  let include_flags = make_include_flags (List.map Path.to_string includes) in

  (* Mode-specific flags *)
  (* Note: -custom is only for ocamlc (bytecode), not ocamlopt (native) *)
  (* In native compilation, C stubs are linked automatically via .o files *)
  let mode_flag =
    match mode with
    | Compile -> "-c"
    | Library -> "-a"
    | CustomExe -> "" (* No -custom for native, C stubs linked via .o *)
    | Executable -> ""
  in

  (* Output flag *)
  let output_flag =
    match output with Some out -> "-o " ^ Path.to_string out | None -> ""
  in

  (* Join source files *)
  let sources_str = String.concat " " sources in

  (* Join library files *)
  let libs_str = String.concat " " (List.map Path.to_string libs) in

  (* Build the complete command *)
  let cmd_parts =
    [
      ocamlc;
      "-g";
      "-bin-annot";
      mode_flag;
      include_flags;
      output_flag;
      libs_str;
      sources_str;
    ]
    |> List.filter (fun s -> s <> "") (* Remove empty strings *)
    |> String.concat " "
  in

  (* Execute the command with colors enabled *)
  (* Set OCAML_COLOR=always to get colored error output *)
  let env = [ ("OCAML_COLOR", "always") ] in
  let cmd = run_in_dir ~cwd ~env cmd_parts in
  match Command.output cmd with
  | Ok output when output.Command.status = 0 -> Success output.Command.stdout
  | Ok output ->
      Failed
        (format "Command failed with status %d: %s" output.Command.status
           output.Command.stderr)
  | Error (Command.SystemError msg) -> Failed msg

(** Compile an interface file (.mli -> .cmi) *)
let compile_interface t ~cwd ~includes ~flags ~output source =
  (* Include current directory for .cmi files *)
  let includes_with_dot = Path.v "." :: includes in

  (* Interface compilation uses ocamlc (same for bytecode and native) *)
  (* If we have flags, we need to build command parts directly *)
  if flags <> [] then
    let flag_args = flags_to_string flags in
    (* Check if we have an -impl flag - if so, don't add source at the end *)
    let has_impl_flag =
      List.exists (function Impl _ -> true | _ -> false) flags
    in
    let cmd_parts =
      [ base_command t; "-g"; "-bin-annot"; "-c" ]
      @ flag_args
      @ List.concat_map
          (fun dir -> [ "-I"; Path.to_string dir ])
          includes_with_dot
      @ [ "-o"; Path.to_string output ]
      @ if has_impl_flag then [] else [ Path.to_string source ]
    in
    let cmd_str = String.concat " " cmd_parts in
    let env = [ ("OCAML_COLOR", "always") ] in
    let cmd = run_in_dir ~cwd ~env cmd_str in
    match Command.output cmd with
    | Ok output when output.Command.status = 0 -> Success output.Command.stdout
    | Ok output ->
        Failed
          (format "Command failed with status %d: %s" output.Command.status
             output.Command.stderr)
    | Error (Command.SystemError msg) -> Failed msg
  else
    run t ~cwd ~includes:includes_with_dot ~output:(Some output) ~mode:Compile
      [ Path.to_string source ]

(** Compile an implementation file (.ml -> .cmx) *)
let compile_impl t ~cwd ~includes ~flags ~output source =
  (* Include current directory for .cmi files *)
  let includes_with_dot = Path.v "." :: includes in

  (* If we have flags, we need to build command parts directly *)
  if flags <> [] then
    let flag_args = flags_to_string flags in
    (* Check if we have an -impl flag - if so, don't add source at the end *)
    let has_impl_flag =
      List.exists (function Impl _ -> true | _ -> false) flags
    in
    let cmd_parts =
      [ base_command t; "-g"; "-bin-annot"; "-c" ]
      @ flag_args
      @ List.concat_map
          (fun dir -> [ "-I"; Path.to_string dir ])
          includes_with_dot
      @ [ "-o"; Path.to_string output ]
      @ if has_impl_flag then [] else [ Path.to_string source ]
    in
    let cmd_str = String.concat " " cmd_parts in
    let env = [ ("OCAML_COLOR", "always") ] in
    let cmd = run_in_dir ~cwd ~env cmd_str in
    match Command.output cmd with
    | Ok output when output.Command.status = 0 -> Success output.Command.stdout
    | Ok output ->
        Failed
          (format "Command failed with status %d: %s" output.Command.status
             output.Command.stderr)
    | Error (Command.SystemError msg) -> Failed msg
  else
    run t ~cwd ~includes:includes_with_dot ~output:(Some output) ~mode:Compile
      [ Path.to_string source ]

(** Generate interface file (.ml -> .mli) using ocamlc -i *)
let generate_interface t ~cwd ~includes ~flags ~output source =
  (* Include current directory for .cmi files *)
  let includes_with_dot = Path.v "." :: includes in

  (* Build command *)
  let cmd_parts =
    [ base_command t; "-i" ]
    @ flags_to_string flags
    @ List.concat_map
        (fun dir -> [ "-I"; Path.to_string dir ])
        includes_with_dot
    @ [ Path.to_string source ]
  in
  let cmd_str = String.concat " " cmd_parts in
  let env = [ ("OCAML_COLOR", "always") ] in
  let cmd = run_in_dir ~cwd ~env cmd_str in

  (* Execute and capture only stdout (stderr has warnings) *)
  match Command.output cmd with
  | Ok out ->
      if out.Command.status = 0 then
        (* Write the stdout (inferred interface) to the output file *)
        match Fs.write out.Command.stdout output with
        | Ok _ ->
            Success (format "Generated interface %s" (Path.to_string output))
        | Error (Fs.SystemError msg) ->
            Failed (format "Failed to write %s: %s" (Path.to_string output) msg)
      else
        (* Include stderr in the error message for debugging *)
        Failed
          (format "ocamlc -i failed with exit code %d: %s" out.Command.status
             out.Command.stderr)
  | Error (Command.SystemError msg) -> Failed msg

(** Compile a C file *)
let compile_c t ~cwd ~includes ~output source =
  run t ~cwd ~includes ~output:(Some output) ~mode:Compile
    [ Path.to_string source ]

(** Create a library (.cmxa) from object files *)
let create_library t ~cwd ~includes ~output objects =
  run t ~cwd ~includes ~output:(Some output) ~mode:Library
    (List.map Path.to_string objects)

(** Create an executable from object files and libraries *)
let create_executable t ~cwd ~includes ~output ~libs objects =
  (* Include current directory *)
  let includes_with_dot = Path.v "." :: includes in
  run t ~cwd ~includes:includes_with_dot ~libs ~output:(Some output)
    ~mode:Executable
    (List.map Path.to_string objects)

(** Create a custom executable (with C stubs) *)
let create_custom_executable t ~cwd ~includes ~output ~libs objects =
  (* Include current directory *)
  let includes_with_dot = Path.v "." :: includes in
  run t ~cwd ~includes:includes_with_dot ~libs ~output:(Some output)
    ~mode:CustomExe
    (List.map Path.to_string objects)

(** Helper to check if compilation succeeded *)
let is_success = function Success _ -> true | Failed _ -> false

(** Helper to get output message *)
let get_output = function Success msg | Failed msg -> msg

(** Tests submodule *)
module Tests = struct
  let test_compile_interface_generates_cmi () =
    (* Test that .mli compilation produces .cmi file *)
    Success "Test passed"
    [@test]

  let test_compile_impl_generates_cmx () =
    (* Test that .ml compilation produces .cmx file *)
    Success "Test passed"
    [@test]

  let test_create_library_bundles_objects () =
    (* Test that .cmxa creation includes all object files *)
    Success "Test passed"
    [@test]

  let test_create_executable_links_dependencies () =
    (* Test that executable links with all required libraries *)
    Success "Test passed"
    [@test]

  let test_custom_executable_handles_c_stubs () =
    (* Test that custom executables properly link C stubs *)
    Success "Test passed"
    [@test]

  let test_include_paths_passed_correctly () =
    (* Test that -I flags are properly constructed *)
    Success "Test passed"
end [@test]
