(** Terminal renderer for Gooey render commands
    
    This module takes render commands from Gooey.layout and draws them
    to the terminal using ANSI escape codes via the Tty package.
    
    {1 Basic Usage}
    
    {[
      open Std
      open Gooey
      
      (* Compute layout *)
      let commands = Gooey.layout ~config my_ui in
      
      (* Render to terminal *)
      Terminal_renderer.render commands;
    ]}
*)

open Std

(** {1 Rendering} *)

val render : Render.command_list -> unit
(** [render commands] draws the render commands to the terminal.
    
    This function:
    - Clears the screen (or updates incrementally)
    - Processes each render command in order
    - Converts colors to ANSI escape codes using Tty
    - Draws rectangles, text, borders
    - Handles clipping regions
    
    @param commands List of render commands from Gooey.layout
*)

val render_to_buffer : Render.command_list -> Buffer.t -> unit
(** [render_to_buffer commands buf] renders to a buffer instead of stdout.
    Useful for testing or custom output handling.
    
    @param commands List of render commands
    @param buf Buffer to write ANSI escape sequences to
*)

(** {1 Configuration} *)

type config = {
  clear_screen : bool;  (** Clear screen before rendering (default: true) *)
  use_alternate_buffer : bool;  (** Use alternate screen buffer (default: false) *)
}

val default_config : config
(** Default rendering configuration *)

val render_with_config : config -> Render.command_list -> unit
(** Render with custom configuration *)

(** {1 Utilities} *)

val clear : unit -> unit
(** Clear the terminal screen *)

val hide_cursor : unit -> unit
(** Hide the cursor *)

val show_cursor : unit -> unit
(** Show the cursor *)

val move_cursor : x:int -> y:int -> unit
(** Move cursor to position *)
