(** Gooey - Declarative layout engine for terminal UIs
    
    Gooey is a pure, stateless layout library inspired by Clay-TUI.
    You compose UI elements declaratively and run the layout algorithm
    to generate render commands.
    
    {1 Basic Usage}
    
    {[
      open Std
      open Gooey
      
      let my_ui =
        Element.column ~style:Style.(empty |> padding (Padding.all 8)) [
          Element.text ~style:Style.(empty |> bold) "Hello, Gooey!";
          Element.row [
            Element.text "Left";
            Element.spacer ~flex:1.0 ();
            Element.text "Right";
          ]
        ]
      
      let text_measurer text style =
        (* Measure text - for terminal, typically character count * font size *)
        Viewport.make 
          ~width:(float_of_int (String.length text) *. 8.0)
          ~height:(float_of_int style.Style.text_size)
      in
      
      let config = Config.make 
        ~viewport:(Viewport.make ~width:80.0 ~height:24.0)
        ~text_measurer
        ()
      in
      
      let commands = Gooey.layout ~config my_ui in
      (* Process render commands... *)
    ]}
*)
open Std

(** Geometric primitives (Point, Rect) *)
module Geometry = Geometry

(** Viewport dimensions *)
module Viewport = Viewport

(** Style configuration and builder API *)
module Style = Style

(** UI element tree *)
module Element = Element

(** Render commands *)
module Render = Render

(** ANSI terminal formatting *)
module Ansi_formatter = Ansi_formatter

(** Terminal renderer *)
module Terminal_renderer_fullscreen = Terminal_renderer_fullscreen

module Terminal_renderer_inline = Terminal_renderer_inline

(** {1 Configuration} *)

(** Function type for measuring text dimensions.
    Takes text content and style, returns dimensions. *)
type text_measurer = string -> Style.t -> Viewport.t
module Config: sig
  type t

  (** Create a layout configuration *)
  val make: viewport:Viewport.t -> text_measurer:text_measurer -> unit -> t

  (** Simple terminal-based text measurement:
      width = character count * 8.0, height = font size *)
  val default_text_measurer: text_measurer
end

(** {1 Layout Algorithm} *)
(** [layout ~config element] computes the layout for [element] using the
    given configuration and returns a list of render commands.
    
    This is a pure function with no side effects.
    
    @param config Layout configuration (viewport, text measurer)
    @param element The root element to layout
    @return List of render commands to draw the UI
*)
val layout: config:Config.t -> Element.t -> Render.command_list
