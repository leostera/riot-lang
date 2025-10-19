open Std
open Tusk_model
open Tusk_ocaml

type t =
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
        (Path.to_string source)
        (Path.to_string output)
        (String.concat "," (List.map Path.to_string includes))
        (String.concat " " (Ocamlc.flags_to_string flags))
  | CompileImplementation { source; output; includes; flags } ->
      format "CompileImplementation(%s->%s,includes=%s,flags=%s)"
        (Path.to_string source)
        (Path.to_string output)
        (String.concat "," (List.map Path.to_string includes))
        (String.concat " " (Ocamlc.flags_to_string flags))
  | GenerateInterface { source; output; includes; flags } ->
      format "GenerateInterface(%s->%s,includes=%s,flags=%s)"
        (Path.to_string source)
        (Path.to_string output)
        (String.concat "," (List.map Path.to_string includes))
        (String.concat " " (Ocamlc.flags_to_string flags))
  | CompileC { source; output } ->
      format "CompileC(%s->%s)" (Path.to_string source) (Path.to_string output)
  | CreateLibrary { output; objects; includes } ->
      format "CreateLibrary(%s,objects=%s,includes=%s)"
        (Path.to_string output)
        (String.concat "," (List.map Path.to_string objects))
        (String.concat "," (List.map Path.to_string includes))
  | CreateExecutable { output; objects; libraries; includes } ->
      format "CreateExecutable(%s,objects=%s,libraries=%s,includes=%s)"
        (Path.to_string output)
        (String.concat "," (List.map Path.to_string objects))
        (String.concat "," (List.map Path.to_string libraries))
        (String.concat "," (List.map Path.to_string includes))
  | CopyFile { source; destination } ->
      format "CopyFile(%s->%s)" (Path.to_string source) (Path.to_string destination)
  | WriteFile { destination; content } ->
      format "WriteFile(%s,%d bytes)" (Path.to_string destination) (String.length content)
