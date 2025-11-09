open Std
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

val hash : t -> Std.Crypto.hash
val to_string : t -> string
val to_json : t -> Data.Json.t
val from_json : Data.Json.t -> (t, string) Result.t
val equal : t -> t -> bool
val outputs : t -> Path.t list
val kind : t -> string
