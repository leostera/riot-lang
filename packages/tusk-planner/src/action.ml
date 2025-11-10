open Std
open Std.Collections
open Tusk_model

type t =
  | CompileInterface of {
      source : Path.t;
      outputs : Path.t list;
      includes : Path.t list;
      flags : Tusk_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileImplementation of {
      source : Path.t;
      outputs : Path.t list;
      includes : Path.t list;
      flags : Tusk_toolchain.Ocamlc.compiler_flag list;
    }
  | GenerateInterface of {
      source : Path.t;
      outputs : Path.t list;
      includes : Path.t list;
      flags : Tusk_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileC of { source : Path.t; outputs : Path.t list }
  | CreateLibrary of {
      outputs : Path.t list;
      objects : Path.t list;
      includes : Path.t list;
    }
  | CreateExecutable of {
      outputs : Path.t list;
      objects : Path.t list;
      libraries : Path.t list;
      includes : Path.t list;
      cclibs : Path.t list;  (* Foreign C/Rust libraries to link with -cclib *)
      ccflags : string list;  (* Additional C compiler/linker flags like -framework *)
    }
  | CopyFile of { source : Path.t; destination : Path.t }
  | WriteFile of { destination : Path.t; content : string }
  | BuildForeignDependency of {
      name : string;
      path : Path.t;
      build_cmd : string list;
      outputs : Path.t list;
      env : (string * string) list;
    }

(** Compute a deterministic content-based hash of an action.

    This hash is used for caching and must be deterministic regardless of:
    - Order of includes in the list
    - Order of objects in the list
    - Order of libraries in the list
    - Order of outputs in the list
    - Order of flags in the list

    All list fields are sorted before hashing to ensure determinism. *)
let hash action =
  let open Crypto in
  let hasher = Sha256.create () in

  (* Helper to write a sorted list of paths *)
  let write_sorted_paths hasher paths =
    let sorted =
      List.sort
        (fun a b -> String.compare (Path.to_string a) (Path.to_string b))
        paths
    in
    List.iter
      (fun path -> Sha256.write_string hasher (Path.to_string path))
      sorted
  in

  (* Helper to write sorted flags - use flags_to_string which returns string list *)
  let write_sorted_flags hasher flags =
    let flag_strings = Tusk_toolchain.Ocamlc.flags_to_string flags in
    let sorted = List.sort String.compare flag_strings in
    List.iter (fun s -> Sha256.write_string hasher s) sorted
  in

  match action with
  | CompileInterface { source; outputs; includes; flags } ->
      Sha256.write_string hasher "CompileInterface";
      Sha256.write_string hasher (Path.to_string source);
      write_sorted_paths hasher outputs;
      write_sorted_paths hasher includes;
      write_sorted_flags hasher flags;
      Sha256.finish hasher
  | CompileImplementation { source; outputs; includes; flags } ->
      Sha256.write_string hasher "CompileImplementation";
      Sha256.write_string hasher (Path.to_string source);
      write_sorted_paths hasher outputs;
      write_sorted_paths hasher includes;
      write_sorted_flags hasher flags;
      Sha256.finish hasher
  | GenerateInterface { source; outputs; includes; flags } ->
      Sha256.write_string hasher "GenerateInterface";
      Sha256.write_string hasher (Path.to_string source);
      write_sorted_paths hasher outputs;
      write_sorted_paths hasher includes;
      write_sorted_flags hasher flags;
      Sha256.finish hasher
  | CompileC { source; outputs } ->
      Sha256.write_string hasher "CompileC";
      Sha256.write_string hasher (Path.to_string source);
      write_sorted_paths hasher outputs;
      Sha256.finish hasher
  | CreateLibrary { outputs; objects; includes } ->
      Sha256.write_string hasher "CreateLibrary";
      write_sorted_paths hasher outputs;
      write_sorted_paths hasher objects;
      write_sorted_paths hasher includes;
      Sha256.finish hasher
  | CreateExecutable { outputs; objects; libraries; includes; cclibs; ccflags } ->
      Sha256.write_string hasher "CreateExecutable";
      write_sorted_paths hasher outputs;
      write_sorted_paths hasher objects;
      write_sorted_paths hasher libraries;
      write_sorted_paths hasher includes;
      write_sorted_paths hasher cclibs;
      List.iter (Sha256.write_string hasher) (List.sort String.compare ccflags);
      Sha256.finish hasher
  | CopyFile { source; destination } ->
      Sha256.write_string hasher "CopyFile";
      Sha256.write_string hasher (Path.to_string source);
      Sha256.write_string hasher (Path.to_string destination);
      Sha256.finish hasher
  | WriteFile { destination; content } ->
      Sha256.write_string hasher "WriteFile";
      Sha256.write_string hasher (Path.to_string destination);
      Sha256.write_string hasher content;
      Sha256.finish hasher
  | BuildForeignDependency { name; path; build_cmd; outputs; env } ->
      Sha256.write_string hasher "BuildForeignDependency";
      Sha256.write_string hasher name;
      Sha256.write_string hasher (Path.to_string path);
      let sorted_cmd = List.sort String.compare build_cmd in
      List.iter (Sha256.write_string hasher) sorted_cmd;
      write_sorted_paths hasher outputs;
      let sorted_env = List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) env in
      List.iter (fun (k, v) -> Sha256.write_string hasher (k ^ "=" ^ v)) sorted_env;
      Sha256.finish hasher

let to_string = function
  | CompileInterface { source; outputs; includes; flags } ->
      "CompileInterface(" ^ Path.to_string source ^ "->" ^ String.concat "," (List.map Path.to_string outputs) ^ ",includes=" ^ String.concat "," (List.map Path.to_string includes) ^ ",flags=" ^ String.concat " " (Tusk_toolchain.Ocamlc.flags_to_string flags) ^ ")"
  | CompileImplementation { source; outputs; includes; flags } ->
      "CompileImplementation(" ^ Path.to_string source ^ "->" ^ String.concat "," (List.map Path.to_string outputs) ^ ",includes=" ^ String.concat "," (List.map Path.to_string includes) ^ ",flags=" ^ String.concat " " (Tusk_toolchain.Ocamlc.flags_to_string flags) ^ ")"
  | GenerateInterface { source; outputs; includes; flags } ->
      "GenerateInterface(" ^ Path.to_string source ^ "->" ^ String.concat "," (List.map Path.to_string outputs) ^ ",includes=" ^ String.concat "," (List.map Path.to_string includes) ^ ",flags=" ^ String.concat " " (Tusk_toolchain.Ocamlc.flags_to_string flags) ^ ")"
  | CompileC { source; outputs } ->
      "CompileC(" ^ Path.to_string source ^ "->" ^ String.concat "," (List.map Path.to_string outputs) ^ ")"
  | CreateLibrary { outputs; objects; includes } ->
      "CreateLibrary(" ^ String.concat "," (List.map Path.to_string outputs) ^ ",objects=" ^ String.concat "," (List.map Path.to_string objects) ^ ",includes=" ^ String.concat "," (List.map Path.to_string includes) ^ ")"
  | CreateExecutable { outputs; objects; libraries; includes; cclibs; ccflags } ->
      "CreateExecutable(" ^ String.concat "," (List.map Path.to_string outputs) ^ ",objects=" ^ String.concat "," (List.map Path.to_string objects) ^ ",libraries=" ^ String.concat "," (List.map Path.to_string libraries) ^ ",includes=" ^ String.concat "," (List.map Path.to_string includes) ^ ",cclibs=" ^ String.concat "," (List.map Path.to_string cclibs) ^ ",ccflags=" ^ String.concat " " ccflags ^ ")"
  | CopyFile { source; destination } ->
      "CopyFile(" ^ Path.to_string source ^ "->" ^ Path.to_string destination ^ ")"
  | WriteFile { destination; content } ->
      "WriteFile(" ^ Path.to_string destination ^ "," ^ Int.to_string (String.length content) ^ " bytes)"
  | BuildForeignDependency { name; path; build_cmd; outputs; _ } ->
      "BuildForeignDependency(" ^ name ^ ",path=" ^ Path.to_string path ^ ",cmd=" ^ String.concat " " build_cmd ^ ",outputs=" ^ String.concat "," (List.map Path.to_string outputs) ^ ")"

let to_json action =
  let open Data.Json in
  match action with
  | CompileInterface { source; outputs; includes; flags } ->
      obj
        [
          ("type", string "CompileInterface");
          ("source", string (Path.to_string source));
          ( "outputs",
            array (List.map (fun p -> string (Path.to_string p)) outputs) );
          ( "includes",
            array (List.map (fun p -> string (Path.to_string p)) includes) );
          ( "flags",
            array
              (List.map string (Tusk_toolchain.Ocamlc.flags_to_string flags)) );
        ]
  | CompileImplementation { source; outputs; includes; flags } ->
      obj
        [
          ("type", string "CompileImplementation");
          ("source", string (Path.to_string source));
          ( "outputs",
            array (List.map (fun p -> string (Path.to_string p)) outputs) );
          ( "includes",
            array (List.map (fun p -> string (Path.to_string p)) includes) );
          ( "flags",
            array
              (List.map string (Tusk_toolchain.Ocamlc.flags_to_string flags)) );
        ]
  | GenerateInterface { source; outputs; includes; flags } ->
      obj
        [
          ("type", string "GenerateInterface");
          ("source", string (Path.to_string source));
          ( "outputs",
            array (List.map (fun p -> string (Path.to_string p)) outputs) );
          ( "includes",
            array (List.map (fun p -> string (Path.to_string p)) includes) );
          ( "flags",
            array
              (List.map string (Tusk_toolchain.Ocamlc.flags_to_string flags)) );
        ]
  | CompileC { source; outputs } ->
      obj
        [
          ("type", string "CompileC");
          ("source", string (Path.to_string source));
          ( "outputs",
            array (List.map (fun p -> string (Path.to_string p)) outputs) );
        ]
  | CreateLibrary { outputs; objects; includes } ->
      obj
        [
          ("type", string "CreateLibrary");
          ( "outputs",
            array (List.map (fun p -> string (Path.to_string p)) outputs) );
          ( "objects",
            array (List.map (fun p -> string (Path.to_string p)) objects) );
          ( "includes",
            array (List.map (fun p -> string (Path.to_string p)) includes) );
        ]
  | CreateExecutable { outputs; objects; libraries; includes; cclibs; ccflags } ->
      obj
        [
          ( "outputs",
            array (List.map (fun p -> string (Path.to_string p)) outputs) );
          ("type", string "CreateExecutable");
          ( "objects",
            array (List.map (fun p -> string (Path.to_string p)) objects) );
          ( "libraries",
            array (List.map (fun p -> string (Path.to_string p)) libraries) );
          ( "includes",
            array (List.map (fun p -> string (Path.to_string p)) includes) );
          ( "cclibs",
            array (List.map (fun p -> string (Path.to_string p)) cclibs) );
          ( "ccflags",
            array (List.map string ccflags) );
        ]
  | CopyFile { source; destination } ->
      obj
        [
          ("type", string "CopyFile");
          ("source", string (Path.to_string source));
          ("destination", string (Path.to_string destination));
        ]
  | WriteFile { destination; content } ->
      obj
        [
          ("type", string "WriteFile");
          ("destination", string (Path.to_string destination));
          ("content", string content);
        ]
  | BuildForeignDependency { name; path; build_cmd; outputs; env } ->
      obj
        [
          ("type", string "BuildForeignDependency");
          ("name", string name);
          ("path", string (Path.to_string path));
          ("build_cmd", array (List.map string build_cmd));
          ("outputs", array (List.map (fun p -> string (Path.to_string p)) outputs));
          ("env", obj (List.map (fun (k, v) -> (k, string v)) env));
        ]

let from_json json =
  let open Data.Json in
  let parse_outputs json =
    match get_field "outputs" json with
    | Some (Array arr) ->
        Some
          (List.filter_map
             (function String s -> Some (Path.v s) | _ -> None)
             arr)
    | _ -> None
  in
  match get_field "type" json with
  | None -> Error "Missing type field"
  | Some (String "CompileInterface") -> (
      match (get_field "source" json, parse_outputs json) with
      | Some (String src), Some outs ->
          Ok
            (CompileInterface
               {
                 source = Path.v src;
                 outputs = outs;
                 includes = [];
                 flags = [];
               })
      | _ -> Error "Invalid CompileInterface")
  | Some (String "CompileImplementation") -> (
      match (get_field "source" json, parse_outputs json) with
      | Some (String src), Some outs ->
          Ok
            (CompileImplementation
               {
                 source = Path.v src;
                 outputs = outs;
                 includes = [];
                 flags = [];
               })
      | _ -> Error "Invalid CompileImplementation")
  | Some (String "GenerateInterface") -> (
      match (get_field "source" json, parse_outputs json) with
      | Some (String src), Some outs ->
          Ok
            (GenerateInterface
               {
                 source = Path.v src;
                 outputs = outs;
                 includes = [];
                 flags = [];
               })
      | _ -> Error "Invalid GenerateInterface")
  | Some (String "CompileC") -> (
      match (get_field "source" json, parse_outputs json) with
      | Some (String src), Some outs ->
          Ok (CompileC { source = Path.v src; outputs = outs })
      | _ -> Error "Invalid CompileC")
  | Some (String "CreateLibrary") -> (
      match parse_outputs json with
      | Some outs ->
          Ok (CreateLibrary { outputs = outs; objects = []; includes = [] })
      | _ -> Error "Invalid CreateLibrary")
  | Some (String "CreateExecutable") -> (
      match parse_outputs json with
      | Some outs ->
          Ok
            (CreateExecutable
               { outputs = outs; objects = []; libraries = []; includes = []; cclibs = []; ccflags = [] })
      | _ -> Error "Invalid CreateExecutable")
  | Some (String "CopyFile") -> (
      match (get_field "source" json, get_field "destination" json) with
      | Some (String src), Some (String dst) ->
          Ok (CopyFile { source = Path.v src; destination = Path.v dst })
      | _ -> Error "Invalid CopyFile")
  | Some (String "WriteFile") -> (
      match (get_field "destination" json, get_field "content" json) with
      | Some (String dst), Some (String content) ->
          Ok (WriteFile { destination = Path.v dst; content })
      | _ -> Error "Invalid WriteFile")
  | Some (String "BuildForeignDependency") -> (
      let parse_build_cmd json =
        match get_field "build_cmd" json with
        | Some (Array arr) ->
            Some (List.filter_map (function String s -> Some s | _ -> None) arr)
        | _ -> None
      in
      let parse_env json =
        match get_field "env" json with
        | Some (Object fields) ->
            List.filter_map
              (fun (k, v) -> match v with String s -> Some (k, s) | _ -> None)
              fields
        | _ -> []
      in
      match (get_field "name" json, get_field "path" json, parse_build_cmd json, parse_outputs json) with
      | Some (String name), Some (String path), Some build_cmd, Some outs ->
          let env = parse_env json in
          Ok (BuildForeignDependency { name; path = Path.v path; build_cmd; outputs = outs; env })
      | _ -> Error "Invalid BuildForeignDependency")
  | Some _ -> Error "Unknown action type"
  | None -> Error "type must be string"

let equal a1 a2 =
  match (a1, a2) with
  | CompileInterface r1, CompileInterface r2 ->
      Path.equal r1.source r2.source
      && List.for_all2 Path.equal r1.outputs r2.outputs
      && List.for_all2 Path.equal r1.includes r2.includes
      && r1.flags = r2.flags
  | CompileImplementation r1, CompileImplementation r2 ->
      Path.equal r1.source r2.source
      && List.for_all2 Path.equal r1.outputs r2.outputs
      && List.for_all2 Path.equal r1.includes r2.includes
      && r1.flags = r2.flags
  | GenerateInterface r1, GenerateInterface r2 ->
      Path.equal r1.source r2.source
      && List.for_all2 Path.equal r1.outputs r2.outputs
      && List.for_all2 Path.equal r1.includes r2.includes
      && r1.flags = r2.flags
  | CompileC r1, CompileC r2 ->
      Path.equal r1.source r2.source
      && List.for_all2 Path.equal r1.outputs r2.outputs
  | CreateLibrary r1, CreateLibrary r2 ->
      List.for_all2 Path.equal r1.outputs r2.outputs
      && List.for_all2 Path.equal r1.objects r2.objects
      && List.for_all2 Path.equal r1.includes r2.includes
  | CreateExecutable r1, CreateExecutable r2 ->
      List.for_all2 Path.equal r1.outputs r2.outputs
      && List.for_all2 Path.equal r1.objects r2.objects
      && List.for_all2 Path.equal r1.libraries r2.libraries
      && List.for_all2 Path.equal r1.includes r2.includes
      && List.for_all2 Path.equal r1.cclibs r2.cclibs
      && List.for_all2 String.equal r1.ccflags r2.ccflags
  | CopyFile r1, CopyFile r2 ->
      Path.equal r1.source r2.source && Path.equal r1.destination r2.destination
  | WriteFile r1, WriteFile r2 ->
      Path.equal r1.destination r2.destination
      && String.equal r1.content r2.content
  | BuildForeignDependency r1, BuildForeignDependency r2 ->
      r1.name = r2.name
      && Path.equal r1.path r2.path
      && r1.build_cmd = r2.build_cmd
      && List.for_all2 Path.equal r1.outputs r2.outputs
      && r1.env = r2.env
  | _ -> false

let outputs = function
  | CompileInterface { outputs; _ } -> outputs
  | CompileImplementation { outputs; _ } -> outputs
  | GenerateInterface { outputs; _ } -> outputs
  | CompileC { outputs; _ } -> outputs
  | CreateLibrary { outputs; _ } -> outputs
  | CreateExecutable { outputs; _ } -> outputs
  | CopyFile { destination; _ } -> [ destination ]
  | WriteFile { destination; _ } -> [ destination ]
  | BuildForeignDependency { outputs; _ } -> outputs

let kind = function
  | CompileInterface _ -> "CompileInterface"
  | CompileImplementation _ -> "CompileImplementation"
  | GenerateInterface _ -> "GenerateInterface"
  | CompileC _ -> "CompileC"
  | CreateLibrary _ -> "CreateLibrary"
  | CreateExecutable _ -> "CreateExecutable"
  | CopyFile _ -> "CopyFile"
  | WriteFile _ -> "WriteFile"
  | BuildForeignDependency _ -> "BuildForeignDependency"
