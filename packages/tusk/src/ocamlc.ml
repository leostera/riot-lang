(** OCaml compiler command generation and execution *)

(** Compilation mode *)
type mode =
  | Compile        (* -c flag *)
  | Library        (* -a flag *)
  | Executable     (* default, no special flag *)
  | CustomExe      (* -custom flag for executables with C stubs *)

(** Compilation result *)
type result = 
  | Success of string
  | Failed of string

(** Generate include flags from directory list *)
let make_include_flags dirs =
  dirs |> List.map (fun dir -> "-I " ^ dir) |> String.concat " "

(** Generate the base ocamlc command with common flags *)
let base_command toolchain_version =
  Toolchains.ocamlc_path toolchain_version

(** Build and run an ocamlc command *)
let run ?(toolchain_version = "5.3.0") 
        ?(includes = []) 
        ?(libs = [])
        ?(output = None)
        ?(mode = Compile)
        ?(verbose = false)
        sources =
  
  let ocamlc = base_command toolchain_version in
  
  (* Always include Unix module path *)
  let all_includes = "+unix" :: includes in
  let include_flags = make_include_flags all_includes in
  
  (* Mode-specific flags *)
  let mode_flag = match mode with
    | Compile -> "-c"
    | Library -> "-a"
    | CustomExe -> "-custom"
    | Executable -> ""
  in
  
  (* Output flag *)
  let output_flag = match output with
    | Some out -> "-o " ^ out
    | None -> ""
  in
  
  (* Join source files *)
  let sources_str = String.concat " " sources in
  
  (* Join library files *)
  let libs_str = String.concat " " libs in
  
  (* Build the complete command *)
  let cmd_parts = [
    ocamlc;
    mode_flag;
    include_flags;
    output_flag;
    libs_str;
    sources_str;
  ] |> List.filter (fun s -> s <> "") (* Remove empty strings *)
    |> String.concat " "
  in
  
  (* Always print the command for visibility *)
  Printf.printf "  $ %s\n" cmd_parts;
  
  (* Execute the command *)
  let success, output = System.run_command cmd_parts in
  if success then Success output else Failed output

(** Compile an interface file (.mli -> .cmi) *)
let compile_interface ~toolchain_version ~includes ~output source =
  run ~toolchain_version 
      ~includes 
      ~output:(Some output)
      ~mode:Compile
      [source]

(** Compile an implementation file (.ml -> .cmo) *)
let compile_impl ~toolchain_version ~includes ~output source =
  (* Always include current directory for .cmi files *)
  let includes_with_dot = "." :: includes in
  run ~toolchain_version 
      ~includes:includes_with_dot
      ~output:(Some output)
      ~mode:Compile
      [source]

(** Compile a C file *)
let compile_c ~toolchain_version ~output source =
  run ~toolchain_version 
      ~output:(Some output)
      ~mode:Compile
      [source]

(** Create a library (.cma) from object files *)
let create_library ~toolchain_version ~includes ~output objects =
  run ~toolchain_version 
      ~includes
      ~output:(Some output)
      ~mode:Library
      objects

(** Create an executable from object files and libraries *)
let create_executable ~toolchain_version ~includes ~output ~libs objects =
  (* Always include current directory and unix.cma *)
  let includes_with_dot = "." :: includes in
  let all_libs = "unix.cma" :: libs in
  run ~toolchain_version 
      ~includes:includes_with_dot
      ~libs:all_libs
      ~output:(Some output)
      ~mode:Executable
      objects

(** Create a custom executable (with C stubs) *)
let create_custom_executable ~toolchain_version ~includes ~output ~libs objects =
  (* Always include current directory and unix.cma *)
  let includes_with_dot = "." :: includes in
  let all_libs = "unix.cma" :: libs in
  run ~toolchain_version 
      ~includes:includes_with_dot
      ~libs:all_libs
      ~output:(Some output)
      ~mode:CustomExe
      objects

(** Helper to check if compilation succeeded *)
let is_success = function
  | Success _ -> true
  | Failed _ -> false

(** Helper to get output message *)
let get_output = function
  | Success msg | Failed msg -> msg