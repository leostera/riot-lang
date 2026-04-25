(**
   Terminal UI primitives for Riot.

   Gooey is terminal-cell based: text measurement, wrapping, clipping, and
   renderer behavior are expressed in visible terminal cells.
*)
module Geometry = Geometry

module Viewport = Viewport

module Style = Style

module Element = Element

module Render = Render

module Config = Config

module Ansi_formatter = Ansi_formatter

module Terminal_renderer_fullscreen = Terminal_renderer_fullscreen

module Terminal_renderer_inline = Terminal_renderer_inline

val layout: config:Config.t -> Element.t -> Render.command list
