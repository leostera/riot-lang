module Raml_compilation = Raml_core.Compilation
module Raml_config = Raml_core.Config
module Raml_event = Raml_core.Event
module Raml_target = Raml_core.Target
module CoreIR = Raml_core.Core_ir
module Compilation = Raml_compilation
module Js = Raml_js.Js
module Native = Raml_native.Native
module Source_unit = Raml_core.Source_unit
module Typ_lowering = Raml_core.Typ_lowering
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
