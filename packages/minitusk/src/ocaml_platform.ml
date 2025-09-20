let stdlib_modules =
  [
    "Array";
    "Buffer";
    "Bytes";
    "Digest";
    "Effect";
    "Filename";
    "Format";
    "Fun";
    "Obj";
    "Int";
    "Printexc";
    "Stdlib";
    "List";
    "Option";
    "String";
    "Sys";
    "Unix";
    "UnixLabels";
  ]

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
  | Impl of string
      (** -impl <file>: Compile <file> as an implementation file *)
  | Warning of compiler_warning list  (** -w: Configure warning flags *)

(** Compilation mode *)
type mode =
  | Compile (* -c flag *)
  | Library (* -a flag *)
  | Executable (* default, no special flag *)
  | CustomExe (* -custom flag for executables with C stubs *)

module Ocamlc = struct
  (** Generate the base ocamlc command from toolchain *)
  let ocamlc_path =
    let home = try Sys.getenv "HOME" with Not_found -> "/Users/ostera" in
    Filename.concat home ".tusk/toolchains/5.3.0/bin/ocamlc"

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
        | Impl file -> acc @ [ "-impl"; file ]
        | Warning warnings ->
            (* Convert warnings to -w flag format *)
            let warning_codes = List.map warning_to_code warnings in
            let warning_str = "-" ^ String.concat "-" warning_codes in
            acc @ [ "-w"; warning_str ])
      [] flags

  (** Build and run an ocamlc command *)
  let run ?(includes = []) ?(libs = []) ?(output = None) ?(mode = Compile)
      ?(verbose = false) sources =
    (* Build command arguments *)
    let args = [ ocamlc_path ] in

    (* Mode-specific flags *)
    let args =
      match mode with
      | Compile -> args @ [ "-c" ]
      | Library -> args @ [ "-a" ]
      | CustomExe -> args @ [ "-custom" ]
      | Executable -> args
    in

    (* Add include directories *)
    let args =
      List.fold_left (fun acc dir -> acc @ [ "-I"; dir ]) args includes
    in

    (* Output flag *)
    let args =
      match output with Some out -> args @ [ "-o"; out ] | None -> args
    in

    (* Add library files *)
    let args = args @ libs in

    (* Add source files *)
    let args = args @ sources in

    (* Execute the command with colors enabled *)
    (* Set OCAML_COLOR=always to get colored error output *)
    Io.run_command_with_output args

  (** Compile an interface file (.mli -> .cmi) *)
  let compile_interface ~includes ~flags ~output source =
    (* Include current directory for .cmi files *)
    let includes_with_dot = "." :: includes in

    (* If we have flags, we need to build command parts directly *)
    if flags <> [] then
      let flag_args = flags_to_string flags in
      (* Check if we have an -impl flag - if so, don't add source at the end *)
      let has_impl_flag =
        List.exists (function Impl _ -> true | _ -> false) flags
      in
      let cmd_parts =
        [ ocamlc_path; "-c" ] @ flag_args
        @ List.concat_map (fun dir -> [ "-I"; dir ]) includes_with_dot
        @ [ "-o"; output ]
        @ if has_impl_flag then [] else [ source ]
      in
      Io.run_command_with_output cmd_parts
    else
      run ~includes:includes_with_dot ~output:(Some output) ~mode:Compile
        [ source ]

  (** Compile an implementation file (.ml -> .cmo) *)
  let compile_impl ~includes ~flags ~output source =
    (* Include current directory for .cmi files *)
    let includes_with_dot = "." :: includes in

    (* If we have flags, we need to build command parts directly *)
    if flags <> [] then
      let flag_args = flags_to_string flags in
      (* Check if we have an -impl flag - if so, don't add source at the end *)
      let has_impl_flag =
        List.exists (function Impl _ -> true | _ -> false) flags
      in
      let cmd_parts =
        [ ocamlc_path; "-c" ] @ flag_args
        @ List.concat_map (fun dir -> [ "-I"; dir ]) includes_with_dot
        @ [ "-o"; output ]
        @ if has_impl_flag then [] else [ source ]
      in
      Io.run_command_with_output cmd_parts
    else
      run ~includes:includes_with_dot ~output:(Some output) ~mode:Compile
        [ source ]

  (** Generate interface file (.ml -> .mli) using ocamlc -i *)
  let generate_interface ~includes ~flags ~output source =
    (* Include current directory for .cmi files *)
    let includes_with_dot = "." :: includes in

    (* Build command using new Command API *)
    let cmd =
      [ ocamlc_path; "-i" ] @ flags_to_string flags
      @ List.concat_map (fun dir -> [ "-I"; dir ]) includes_with_dot
      @ [ source ]
    in

    (* Execute and capture only stdout (stderr has warnings) *)
    match Io.run_command_with_output cmd with
    | Ok stdout ->
        (* Write the stdout (inferred interface) to the output file *)
        Io.write_file output stdout;
        Ok ()
    | Error err -> Error err

  (** Compile a C file *)
  let compile_c ~includes ~output source =
    run ~includes ~output:(Some output) ~mode:Compile [ source ]

  (** Create a library (.cma) from object files *)
  let create_library ~includes ~output objects =
    run ~includes ~output:(Some output) ~mode:Library objects

  (** Create an executable from object files and libraries *)
  let create_executable ~includes ~output ~libs objects =
    (* Include current directory *)
    let includes_with_dot = "." :: includes in
    run ~includes:includes_with_dot ~libs ~output:(Some output) ~mode:Executable
      objects

  (** Create a custom executable (with C stubs) *)
  let create_custom_executable ~includes ~output ~libs objects =
    (* Include current directory *)
    let includes_with_dot = "." :: includes in
    run ~includes:includes_with_dot ~libs ~output:(Some output) ~mode:CustomExe
      objects
end

module Ocamldep = struct
  let ocamldep_path =
    let home = try Sys.getenv "HOME" with Not_found -> "/Users/ostera" in
    Filename.concat home ".tusk/toolchains/5.3.0/bin/ocamldep"

  let skip_stdlib deps =
    List.filter (fun dep -> not (List.mem dep stdlib_modules)) deps

  (** Parse ocamldep output to extract module names *)
  let parse_deps line =
    (* Format: "file.ml: Module1 Module2 Module3" *)
    match String.split_on_char ':' line with
    | [ _file; deps_str ] ->
        let deps = String.trim deps_str in
        if deps = "" then []
        else String.split_on_char ' ' deps |> List.map String.trim
    | _ -> []

  (** Run ocamldep to get module dependencies for a file *)
  let get_deps ?(includes = []) ?(open_modules = []) source =
    (* Build command arguments *)
    let args = [ ocamldep_path; "-modules" ] in

    (* Add include directories *)
    let args =
      List.fold_left (fun acc dir -> acc @ [ "-I"; dir ]) args includes
    in

    (* Add open modules *)
    let args =
      List.fold_left (fun acc m -> acc @ [ "-open"; m ]) args open_modules
    in

    (* Add source file *)
    let cmd = args @ [ source ] in

    match Io.run_command_with_output cmd with
    | Ok output -> (
        (* Get the first line of output *)
        let lines = String.split_on_char '\n' output in
        match lines with
        | line :: _ when line <> "" -> parse_deps line |> skip_stdlib
        | _ -> [])
    | Error _ -> []

  (** Sort files in dependency order *)
  let sort_files ?(includes = []) files =
    if files = [] then []
    else
      (* Build command arguments *)
      let args = [ ocamldep_path; "-sort" ] in

      (* Add include directories *)
      let args =
        List.fold_left (fun acc dir -> acc @ [ "-I"; dir ]) args includes
      in

      (* Add files *)
      let cmd = args @ files in

      match Io.run_command_with_output cmd with
      | Ok output -> (
          (* Get first non-empty line *)
          let lines = String.split_on_char '\n' output in
          match lines with
          | sorted_str :: _ when sorted_str <> "" ->
              (* ocamldep returns full paths, convert back to basenames *)
              String.split_on_char ' ' sorted_str
              |> List.filter_map (fun s ->
                  if s = "" then None else Some (Filename.basename s))
          | _ -> files)
      | Error _ -> files
end
