open Std

val rewrite_ocamlc_result:
  package:Tusk_model.Package.t ->
  sandbox_dir:Path.t ->
  Tusk_toolchain.Ocamlc.result ->
  Tusk_toolchain.Ocamlc.result
