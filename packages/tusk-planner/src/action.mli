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

val to_string : t -> string
