module Raml_compilation = Compilation
module Raml_config = Config
module Raml_event = Event
module Raml_target = Target
open Std
module Core_ir = Core_ir
module Compilation = Raml_compilation
module Core_ir_fixture_support = Core_ir_fixture_support
module Example_pipeline = Example_pipeline
module Js = Js
module Native = Native
module Source_unit = Source_unit
module Typ_lowering = Typ_lowering
module Config = Raml_config
module Event = Raml_event
module Target = Raml_target

let compile_source = Raml_driver.compile_source

let compile = Raml_driver.compile
