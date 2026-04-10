module Raml_config = Config

module Raml_compilation = Compilation

module Raml_example_pipeline = Example_pipeline

open Std

val compile_source:
  ?config:Raml_config.t -> relpath:Path.t -> string -> (Raml_compilation.t, string) result

val compile: ?config:Raml_config.t -> Path.t -> (Raml_compilation.t, string) result
