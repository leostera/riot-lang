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

(** Helper: Create a cell with just a character *)
val char : string -> cell

(** Helper: Create a cell with character and foreground color *)
val char_fg : string -> Tty.Color.t -> cell

(** Helper: Create a cell with character and background color *)
val char_bg : string -> Tty.Color.t -> cell

(** Helper: Create a cell with character and both colors *)
val char_fg_bg : string -> Tty.Color.t -> Tty.Color.t -> cell

(** Helper: Create a cell with character and all style attributes *)
val char_styled : string -> 
  ?fg:Tty.Color.t option ->
  ?bg:Tty.Color.t option ->
  ?bold:bool ->
  ?italic:bool ->
  ?underline:bool ->
  ?strikethrough:bool ->
  ?reverse:bool ->
  unit -> cell

(** Create a matrix from a 2D array of strings (each string is a character) *)
val of_char_array : string array array -> t

(** Create a matrix from a 2D array of cells *)
val of_cell_array : cell array array -> t

(** Compare two cells for equality *)
val cell_equal : cell -> cell -> bool

(** Compare two matrices for equality *)
val equal : t -> t -> bool

(** Get a human-readable diff between two matrices *)
val diff : t -> t -> string
