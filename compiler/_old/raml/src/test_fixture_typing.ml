(* NOTE: test support only. This stays internal to the package so fixture typing
   support does not leak through the public Raml API. *)

module Compiler_config = Raml_core.Config

let raml_config = fun ~host ~target -> Compiler_config.make ~host ~target ()
