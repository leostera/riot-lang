open Std
open Std.Collections

(** OCaml compiler command generation and execution *)
type t = Path.t

type compiler_warning =
  | NoCmiFile
  | All

type compiler_flag =
  | NoAliasDeps
  | Open of string
  | NoStdlib
  | NoPervasives
  | Impl of Std.Path.t
  | Warning of compiler_warning list
  | LinkAll

type result =
  Success of string
  | Failed of string

type mode =
  | Compile
  | Library
  | Executable
  | CustomExe
  | SharedLibrary

type output_mode =
  | Normal
  | WriteStdoutToFile of Path.t

type invocation = {
  cwd : Path.t;
  env : (string * string) list;
  command_string : string;
  output_mode : output_mode;
}

let make = fun path -> path

let path = fun t -> t

let run_in_dir = fun ~cwd ~env cmd_str ->
  let cmd_with_cd = "cd " ^ Path.to_string cwd ^ " && " ^ cmd_str in
  Log.debug ("  $ " ^ cmd_with_cd);
  Command.make ~env ~args:[ "-c"; cmd_with_cd ] "sh"

let base_command = fun t -> Path.to_string t

let warning_to_code =
  function
  | NoCmiFile -> "49"
  | All -> "a"

let flags_to_string = fun flags ->
  List.fold_left
    (fun acc flag ->
      match flag with
      | Open m ->
          acc @ [ "-open"; m ]
      | NoAliasDeps ->
          acc @ [ "-no-alias-deps" ]
      | NoStdlib ->
          acc @ [ "-nostdlib" ]
      | NoPervasives ->
          acc @ [ "-nopervasives" ]
      | LinkAll ->
          acc @ [ "-linkall" ]
      | Impl file ->
          acc @ [ "-impl"; Path.to_string file ]
      | Warning warnings ->
          let warning_codes = List.map warning_to_code warnings in
          let warning_str = "-" ^ String.concat "-" warning_codes in
          acc @ [ "-w"; warning_str ])
    []
    flags

let make_include_flags = fun dirs -> dirs |> List.map (fun dir -> "-I " ^ dir) |> String.concat " "

let make_invocation = fun ?(output_mode = Normal) ~cwd command_string ->
  {cwd; env = [ ("OCAML_COLOR", "always") ]; command_string; output_mode; }

let build_invocation = fun t ~cwd ?(includes = []) ?(libs = []) ?(cclibs = []) ?(ccflags = []) ?(ccopt_flags = []) ?(cclib_flags = []) ?(output = None) ?(mode = Compile) ?(flags = []) sources ->
  let ocamlc = base_command t in
  let include_flags = make_include_flags (List.map Path.to_string includes) in
  let mode_flag =
    match mode with
    | Compile -> "-c"
    | Library -> "-a"
    | CustomExe -> ""
    | Executable -> ""
    | SharedLibrary -> "-shared"
  in
  let output_flag =
    match output with
    | Some out -> "-o " ^ Path.to_string out
    | None -> ""
  in
  let sources_str = String.concat " " sources in
  let libs_str = String.concat " " (List.map Path.to_string libs) in
  let cclibs_str =
    if List.length cclibs > 0 then
      String.concat " " (List.map (fun lib -> "-cclib " ^ Path.to_string lib) cclibs)
    else
      ""
  in
  let ccflags_str =
    if List.length ccflags > 0 then
      String.concat " " (List.map (fun flag -> "-ccopt \"" ^ flag ^ "\"") ccflags)
    else
      ""
  in
  let ccopt_flags_str =
    if List.length ccopt_flags > 0 then
      String.concat " " (List.map (fun flag -> "-ccopt \"" ^ flag ^ "\"") ccopt_flags)
    else
      ""
  in
  let cclib_flags_str =
    if List.length cclib_flags > 0 then
      String.concat " " (List.map (fun flag -> "-cclib \"" ^ flag ^ "\"") cclib_flags)
    else
      ""
  in
  let flags_str = String.concat " " (flags_to_string flags) in
  let command_string =
    [
      ocamlc;
      "-g";
      "-bin-annot";
      mode_flag;
      flags_str;
      include_flags;
      output_flag;
      libs_str;
      cclibs_str;
      ccflags_str;
      ccopt_flags_str;
      cclib_flags_str;
      sources_str;

    ]
    |> List.filter (fun s -> s != "")
    |> String.concat " "
  in
  if mode = SharedLibrary then
    begin
      Log.info
      ("[OCAMLC] Building shared library with includes: "
      ^ String.concat ", " (List.map Path.to_string includes));
      Log.info ("[OCAMLC] cclibs: " ^ String.concat ", " (List.map Path.to_string cclibs));
      Log.info ("[OCAMLC] objects/sources: " ^ sources_str);
      Log.info ("[OCAMLC] Full command: " ^ command_string)
    end;
  make_invocation ~cwd command_string

let compile_interface = fun t ~cwd ~includes ~flags ~output source ->
  let includes_with_dot = Path.v "." :: includes in
  let has_impl_flag =
    List.exists
      (
        function
        | Impl _ -> true
        | _ -> false
      )
      flags
  in
  let args = [ "-g"; "-bin-annot"; "-c" ]
  @ flags_to_string flags
  @ List.concat_map (fun dir -> [ "-I"; Path.to_string dir ]) includes_with_dot
  @ [ "-o"; Path.to_string output ]
  @ if has_impl_flag then
    []
  else
    [ Path.to_string source ]
  in
  make_invocation ~cwd (String.concat " " ([ base_command t ] @ args))

let compile_impl = fun t ~cwd ~includes ~flags ~output source ->
  let includes_with_dot = Path.v "." :: includes in
  let has_impl_flag =
    List.exists
      (
        function
        | Impl _ -> true
        | _ -> false
      )
      flags
  in
  let args = [ "-g"; "-bin-annot"; "-c" ]
  @ flags_to_string flags
  @ List.concat_map (fun dir -> [ "-I"; Path.to_string dir ]) includes_with_dot
  @ [ "-o"; Path.to_string output ]
  @ if has_impl_flag then
    []
  else
    [ Path.to_string source ]
  in
  make_invocation ~cwd (String.concat " " ([ base_command t ] @ args))

let generate_interface = fun t ~cwd ~includes ~flags ~output source ->
  let includes_with_dot = Path.v "." :: includes in
  let args = [ "-i" ]
  @ flags_to_string flags
  @ List.concat_map (fun dir -> [ "-I"; Path.to_string dir ]) includes_with_dot
  @ [ Path.to_string source ] in
  make_invocation
  ~output_mode:(WriteStdoutToFile output)
  ~cwd
  (String.concat " " ([ base_command t ] @ args))

let compile_c = fun t ~cwd ~includes ?(ccflags = []) ~output source ->
  build_invocation
  t
  ~cwd
  ~includes
  ~ccflags
  ~output:(Some output)
  ~mode:Compile [ Path.to_string source ]

let create_library = fun t ~cwd ~includes ~output objects ->
  build_invocation
  t
  ~cwd
  ~includes
  ~output:(Some output)
  ~mode:Library (List.map Path.to_string objects)

let create_executable = fun t ~cwd ~includes ~output ~libs ?(cclibs = []) ?(ccopt_flags = []) ?(cclib_flags = []) objects ->
  let includes_with_dot = Path.v "." :: includes in
  build_invocation
  t
  ~cwd
  ~includes:includes_with_dot
  ~libs
  ~cclibs
  ~ccopt_flags
  ~cclib_flags
  ~output:(Some output)
  ~mode:Executable
  ~flags:[ LinkAll ]
  (List.map Path.to_string objects)

let create_shared_library = fun t ~cwd ~includes ~output ~libs ?(cclibs = []) ?(ccopt_flags = []) ?(cclib_flags = []) objects ->
  let includes_with_dot = Path.v "." :: includes in
  build_invocation
  t
  ~cwd
  ~includes:includes_with_dot
  ~libs
  ~cclibs
  ~ccopt_flags
  ~cclib_flags
  ~output:(Some output)
  ~mode:SharedLibrary
  ~flags:[ LinkAll ]
  (List.map Path.to_string objects)

let create_custom_executable = fun t ~cwd ~includes ~output ~libs objects ->
  let includes_with_dot = Path.v "." :: includes in
  build_invocation
  t
  ~cwd
  ~includes:includes_with_dot
  ~libs
  ~output:(Some output)
  ~mode:CustomExe (List.map Path.to_string objects)

let to_string = fun invocation ->
  let env_prefix =
    match invocation.env with
    | [] -> ""
    | env -> String.concat " " (List.map (fun ((key, value)) -> key ^ "=" ^ value) env) ^ " "
  in
  "cd " ^ Path.to_string invocation.cwd ^ " && " ^ env_prefix ^ invocation.command_string

let run = fun invocation ->
  Log.debug ("[OCAMLC] Running command: " ^ to_string invocation);
  let cmd = run_in_dir ~cwd:invocation.cwd ~env:invocation.env invocation.command_string in
  match Command.output cmd with
  | Ok output when output.Command.status = 0 -> (
      match invocation.output_mode with
      | Normal -> Success output.Command.stdout
      | WriteStdoutToFile file -> (
          match Fs.write output.Command.stdout file with
          | Ok () -> Success ("Generated interface " ^ Path.to_string file)
          | Error err -> Failed ("Failed to write " ^ Path.to_string file ^ ": " ^ IO.error_message err)
        )
    )
  | Ok output -> (
      match invocation.output_mode with
      | Normal -> Failed ("Command failed with status "
      ^ Int.to_string output.Command.status
      ^ ": "
      ^ output.Command.stderr)
      | WriteStdoutToFile _ -> Failed ("ocamlc -i failed with exit code "
      ^ Int.to_string output.Command.status
      ^ ": "
      ^ output.Command.stderr)
    )
  | Error (Command.SystemError msg) ->
      Failed msg

let is_success =
  function
  | Success _ -> true
  | Failed _ -> false

let get_output =
  function
  | Success msg
  | Failed msg -> msg
