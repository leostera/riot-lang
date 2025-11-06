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

(** {1 Core Modules} *)

module Geometry = Geometry
(** Geometric primitives (Point, Rect) *)

module Viewport = Viewport
(** Viewport dimensions *)

module Style = Style
(** Style configuration and builder API *)

module Element = Element
(** UI element tree *)

module Render = Render
(** Render commands *)

module Ansi_formatter = Ansi_formatter
(** ANSI terminal formatting *)

module Terminal_renderer = Terminal_renderer
(** Terminal renderer *)

(** {1 Configuration} *)

type text_measurer = string -> Style.t -> Viewport.t
(** Function type for measuring text dimensions.
    Takes text content and style, returns dimensions. *)

module Config : sig
  type t
  
  val make : 
    viewport:Viewport.t ->
    text_measurer:text_measurer ->
    unit ->
    t
  (** Create a layout configuration *)
  
  val default_text_measurer : text_measurer
  (** Simple terminal-based text measurement:
      width = character count * 8.0, height = font size *)
end

(** {1 Layout Algorithm} *)

val layout : config:Config.t -> Element.t -> Render.command_list
(** [layout ~config element] computes the layout for [element] using the
    given configuration and returns a list of render commands.
    
    This is a pure function with no side effects.
    
    @param config Layout configuration (viewport, text measurer)
    @param element The root element to layout
    @return List of render commands to draw the UI
*)
