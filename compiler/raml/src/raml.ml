module Raml_compilation = RamlCore.Compilation
module Raml_config = RamlCore.Config
module Raml_event = RamlCore.Event
module Raml_target = RamlCore.Target

module CoreIR = RamlCore.CoreIR
module Compilation = Raml_compilation
module Js = Js
module Native = RamlNative.Native
module Source_unit = RamlCore.Source_unit
module Typ_lowering = RamlCore.Typ_lowering
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
