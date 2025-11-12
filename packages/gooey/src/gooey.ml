open Std

module Geometry = Geometry
module Viewport = Viewport
module Style = Style
module Element = Element
module Render = Render
module Config = Config
module Ansi_formatter = Ansi_formatter
module Terminal_renderer_fullscreen = Terminal_renderer_fullscreen
module Terminal_renderer_inline = Terminal_renderer_inline

type text_measurer = Config.text_measurer

let layout ~config element = Layout.compute ~config element
