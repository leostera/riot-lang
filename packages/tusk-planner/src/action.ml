open Std
open Tusk_model

type t =
  | CompileInterface of {
      source : Path.t;
      output : Path.t;
      includes : Path.t list;
      flags : Tusk_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileImplementation of {
      source : Path.t;
      output : Path.t;
      includes : Path.t list;
      flags : Tusk_toolchain.Ocamlc.compiler_flag list;
    }
  | GenerateInterface of {
      source : Path.t;
      output : Path.t;
      includes : Path.t list;
      flags : Tusk_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileC of { source : Path.t; output : Path.t }
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
  | CopyFile of { source : Path.t; destination : Path.t }
  | WriteFile of { destination : Path.t; content : string }

let to_string = function
  | CompileInterface { source; output; includes; flags } ->
      format "CompileInterface(%s->%s,includes=%s,flags=%s)"
        (Path.to_string source) (Path.to_string output)
        (String.concat "," (List.map Path.to_string includes))
        (String.concat " " (Tusk_toolchain.Ocamlc.flags_to_string flags))
  | CompileImplementation { source; output; includes; flags } ->
      format "CompileImplementation(%s->%s,includes=%s,flags=%s)"
        (Path.to_string source) (Path.to_string output)
        (String.concat "," (List.map Path.to_string includes))
        (String.concat " " (Tusk_toolchain.Ocamlc.flags_to_string flags))
  | GenerateInterface { source; output; includes; flags } ->
      format "GenerateInterface(%s->%s,includes=%s,flags=%s)"
        (Path.to_string source) (Path.to_string output)
        (String.concat "," (List.map Path.to_string includes))
        (String.concat " " (Tusk_toolchain.Ocamlc.flags_to_string flags))
  | CompileC { source; output } ->
      format "CompileC(%s->%s)" (Path.to_string source) (Path.to_string output)
  | CreateLibrary { output; objects; includes } ->
      format "CreateLibrary(%s,objects=%s,includes=%s)" (Path.to_string output)
        (String.concat "," (List.map Path.to_string objects))
        (String.concat "," (List.map Path.to_string includes))
  | CreateExecutable { output; objects; libraries; includes } ->
      format "CreateExecutable(%s,objects=%s,libraries=%s,includes=%s)"
        (Path.to_string output)
        (String.concat "," (List.map Path.to_string objects))
        (String.concat "," (List.map Path.to_string libraries))
        (String.concat "," (List.map Path.to_string includes))
  | CopyFile { source; destination } ->
      format "CopyFile(%s->%s)" (Path.to_string source)
        (Path.to_string destination)
  | WriteFile { destination; content } ->
      format "WriteFile(%s,%d bytes)"
        (Path.to_string destination)
        (String.length content)

let to_json action =
  let open Data.Json in
  match action with
  | CompileInterface { source; output; includes; flags } ->
      obj
        [
          ("type", string "CompileInterface");
          ("source", string (Path.to_string source));
          ("output", string (Path.to_string output));
          ( "includes",
            array (List.map (fun p -> string (Path.to_string p)) includes) );
          ( "flags",
            array
              (List.map string (Tusk_toolchain.Ocamlc.flags_to_string flags)) );
        ]
  | CompileImplementation { source; output; includes; flags } ->
      obj
        [
          ("type", string "CompileImplementation");
          ("source", string (Path.to_string source));
          ("output", string (Path.to_string output));
          ( "includes",
            array (List.map (fun p -> string (Path.to_string p)) includes) );
          ( "flags",
            array
              (List.map string (Tusk_toolchain.Ocamlc.flags_to_string flags)) );
        ]
  | GenerateInterface { source; output; includes; flags } ->
      obj
        [
          ("type", string "GenerateInterface");
          ("source", string (Path.to_string source));
          ("output", string (Path.to_string output));
          ( "includes",
            array (List.map (fun p -> string (Path.to_string p)) includes) );
          ( "flags",
            array
              (List.map string (Tusk_toolchain.Ocamlc.flags_to_string flags)) );
        ]
  | CompileC { source; output } ->
      obj
        [
          ("type", string "CompileC");
          ("source", string (Path.to_string source));
          ("output", string (Path.to_string output));
        ]
  | CreateLibrary { output; objects; includes } ->
      obj
        [
          ("type", string "CreateLibrary");
          ("output", string (Path.to_string output));
          ( "objects",
            array (List.map (fun p -> string (Path.to_string p)) objects) );
          ( "includes",
            array (List.map (fun p -> string (Path.to_string p)) includes) );
        ]
  | CreateExecutable { output; objects; libraries; includes } ->
      obj
        [
          ("output", string (Path.to_string output));
          ("type", string "CreateExecutable");
          ( "objects",
            array (List.map (fun p -> string (Path.to_string p)) objects) );
          ( "libraries",
            array (List.map (fun p -> string (Path.to_string p)) libraries) );
          ( "includes",
            array (List.map (fun p -> string (Path.to_string p)) includes) );
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

let from_json json =
  let open Data.Json in
  match get_field "type" json with
  | None -> Error "Missing type field"
  | Some (String "CompileInterface") -> (
      match (get_field "source" json, get_field "output" json) with
      | Some (String src), Some (String out) ->
          Ok
            (CompileInterface
               {
                 source = Path.v src;
                 output = Path.v out;
                 includes = [];
                 flags = [];
               })
      | _ -> Error "Invalid CompileInterface")
  | Some (String "CompileImplementation") -> (
      match (get_field "source" json, get_field "output" json) with
      | Some (String src), Some (String out) ->
          Ok
            (CompileImplementation
               {
                 source = Path.v src;
                 output = Path.v out;
                 includes = [];
                 flags = [];
               })
      | _ -> Error "Invalid CompileImplementation")
  | Some (String "GenerateInterface") -> (
      match (get_field "source" json, get_field "output" json) with
      | Some (String src), Some (String out) ->
          Ok
            (GenerateInterface
               {
                 source = Path.v src;
                 output = Path.v out;
                 includes = [];
                 flags = [];
               })
      | _ -> Error "Invalid GenerateInterface")
  | Some (String "CompileC") -> (
      match (get_field "source" json, get_field "output" json) with
      | Some (String src), Some (String out) ->
          Ok (CompileC { source = Path.v src; output = Path.v out })
      | _ -> Error "Invalid CompileC")
  | Some (String "CreateLibrary") -> (
      match get_field "output" json with
      | Some (String out) ->
          Ok
            (CreateLibrary { output = Path.v out; objects = []; includes = [] })
      | _ -> Error "Invalid CreateLibrary")
  | Some (String "CreateExecutable") -> (
      match get_field "output" json with
      | Some (String out) ->
          Ok
            (CreateExecutable
               {
                 output = Path.v out;
                 objects = [];
                 libraries = [];
                 includes = [];
               })
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
  | Some _ -> Error "Unknown action type"
  | None -> Error "type must be string"

let equal a1 a2 =
  match (a1, a2) with
  | CompileInterface r1, CompileInterface r2 ->
      Path.equal r1.source r2.source
      && Path.equal r1.output r2.output
      && List.for_all2 Path.equal r1.includes r2.includes
      && r1.flags = r2.flags
  | CompileImplementation r1, CompileImplementation r2 ->
      Path.equal r1.source r2.source
      && Path.equal r1.output r2.output
      && List.for_all2 Path.equal r1.includes r2.includes
      && r1.flags = r2.flags
  | GenerateInterface r1, GenerateInterface r2 ->
      Path.equal r1.source r2.source
      && Path.equal r1.output r2.output
      && List.for_all2 Path.equal r1.includes r2.includes
      && r1.flags = r2.flags
  | CompileC r1, CompileC r2 ->
      Path.equal r1.source r2.source && Path.equal r1.output r2.output
  | CreateLibrary r1, CreateLibrary r2 ->
      Path.equal r1.output r2.output
      && List.for_all2 Path.equal r1.objects r2.objects
      && List.for_all2 Path.equal r1.includes r2.includes
  | CreateExecutable r1, CreateExecutable r2 ->
      Path.equal r1.output r2.output
      && List.for_all2 Path.equal r1.objects r2.objects
      && List.for_all2 Path.equal r1.libraries r2.libraries
      && List.for_all2 Path.equal r1.includes r2.includes
  | CopyFile r1, CopyFile r2 ->
      Path.equal r1.source r2.source && Path.equal r1.destination r2.destination
  | WriteFile r1, WriteFile r2 ->
      Path.equal r1.destination r2.destination
      && String.equal r1.content r2.content
  | _ -> false
