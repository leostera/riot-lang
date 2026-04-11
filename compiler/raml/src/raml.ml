module Raml_compilation = Compilation
module Raml_config = Config
module Raml_event = Event
module Raml_target = Target

module CoreIR = Core_ir
module Compilation = Raml_compilation
module Js = Js
module Native = Native
module Source_unit = Source_unit
module Typ_lowering = Typ_lowering
module Config = Raml_config
module Event = Raml_event
module Target = Raml_target

let compile = Raml_driver.compile

module TestingHelpers = struct
  let compile_source = Raml_driver.compile_source

  module Test_fixture_typing = Test_fixture_typing
  module Example_pipeline = Example_pipeline
  module Core_ir_fixture_support = Core_ir_fixture_support
end
