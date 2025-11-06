open Std

module Geometry = Geometry
module Viewport = Viewport
module Style = Style
module Element = Element
module Render = Render
module Config = Config
module Ansi_formatter = Ansi_formatter
module Terminal_renderer = Terminal_renderer

type text_measurer = Config.text_measurer

let layout ~config element = Layout.compute ~config element
