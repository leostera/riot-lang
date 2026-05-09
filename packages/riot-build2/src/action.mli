open Std

type compile_library_source_kind =
  | LibraryInterface
  | LibraryImplementation

type compile_library_source = {
  source: Path.t;
  staged: Path.t;
  kind: compile_library_source_kind;
  content: string option;
  opens: string list;
}

type t =
  | CompileC of {
      source: Path.t;
      outputs: Path.t list;
      ccflags: string list;
    }
  | CompileLibrary of {
      sources: compile_library_source list;
      objects: Path.t list;
      outputs: Path.t list;
      output: Path.t;
      includes: Path.t list;
      flags: Riot_toolchain.Ocamlc.compiler_flag list;
    }
  | CopyFile of {
      source: Path.t;
      destination: Path.t;
    }
  | WriteFile of {
      destination: Path.t;
      content: string;
    }

val outputs: t -> Path.t list

val export_outputs: t -> Path.t list

val requires_toolchain: t -> bool

val kind: t -> string

val hash: package:Riot_model.Package.t -> toolchain:Riot_toolchain.t -> t -> Crypto.hash

val serialize: t Serde.Ser.t

val deserialize: t Serde.De.t
