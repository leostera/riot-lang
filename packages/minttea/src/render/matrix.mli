(** Matrix - 2D grid of styled cells for terminal rendering *)

open Std

(** A cell in the terminal grid *)
type cell = {
  char : string;  (** UTF-8 character *)
  fg : Tty.Color.t option;
  bg : Tty.Color.t option;
  bold : bool;
  italic : bool;
  underline : bool;
  strikethrough : bool;
  reverse : bool;
}

(** Empty cell (space character, no styling) *)
val empty_cell : cell

(** Matrix representing the terminal screen *)
type t = {
  width : int;
  height : int;
  cells : cell array array;  (** [row][col] *)
}

(** Create a new matrix filled with empty cells *)
val create : width:int -> height:int -> t

(** Get cell at position (bounds-checked) *)
val get : t -> x:int -> y:int -> cell option

(** Set cell at position (bounds-checked, returns unit) *)
val set : t -> x:int -> y:int -> cell -> unit

(** Fill a rectangle with a character and style *)
val fill_rect : t -> x:int -> y:int -> width:int -> height:int -> cell -> unit

(** Write text at position with style (wraps to fit) *)
val write_text : t -> x:int -> y:int -> max_width:int -> string -> cell -> unit

(** Clear the entire matrix (fill with empty cells) *)
val clear : t -> unit

(** Create a copy of the matrix *)
val copy : t -> t
