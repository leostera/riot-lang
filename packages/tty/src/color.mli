(**
   Terminal color representation and manipulation.

   Supports three color modes:
   - RGB: 24-bit true color (16.7 million colors)
   - ANSI256: 256-color palette
   - ANSI: Basic 16-color palette
   - No_color: Disable colors

   ## Examples

   Creating colors:
   ```ocaml
   (* From hex strings *)
   let red = Color.make "#FF0000"
   let blue = Color.make "#00F"

   (* From RGB tuples *)
   let green = Color.from_rgb (0, 255, 0)

   (* ANSI basic colors (0-15) *)
   let yellow = Color.ansi 3

   (* ANSI 256-color palette *)
   let orange = Color.ansi256 214

   (* Disable colors *)
   let none = Color.no_color
   ```

   Using colors in terminal output:
   ```ocaml
   let red = Color.make "#FF0000"
   let fg_red = Color.to_escape_seq ~mode:`fg red
   let bg_red = Color.to_escape_seq ~mode:`bg red

   Printf.printf "\027[%smRed text\027[0m\n" fg_red;
   Printf.printf "\027[%smRed background\027[0m\n" bg_red
   ```

   Checking color types:
   ```ocaml
   let color = Color.make "#FF0000"
   if Color.is_rgb color then
     print_endline "This is an RGB color"
   ```
*)
type t =
  private | RGB of int * int * int
  (** 24-bit RGB color (r, g, b) where each component is 0-255 *)
  | ANSI of int
  (** Basic ANSI color (0-15) *)
  | ANSI256 of int
  (** Extended ANSI 256-color palette (0-255) *)
  | No_color

(** No color / disabled *)
val make: string -> t

(**
   Parse color from string.

   Accepts:
   - Hex colors: "#FF0000" or "#F00"
   - ANSI codes: "0" through "15" for basic colors
   - ANSI256 codes: "16" through "255"

   Examples: ```ocaml Color.make "#FF0000" (* Hex RGB *) Color.make "#F00" (*
   Short hex *) Color.make "4" (* ANSI blue *) Color.make "196" (* ANSI256 red
   *) ```

   @raise Invalid_color if string format is invalid
*)
val from_rgb: int * int * int -> t

(**
   Create RGB color from (red, green, blue) tuple.

   Components are clamped to 0-255 range.

   Examples: ```ocaml Color.from_rgb (255, 0, 0) (* Red *) Color.from_rgb (0, 255,
   0) (* Green *) Color.from_rgb (300, -10, 128) (* Clamped to (255, 0, 128) *)
   ```
*)
val ansi: int -> t

(**
   Create basic ANSI color (0-15).

   Standard colors:
   - 0: Black
   - 1: Red
   - 2: Green
   - 3: Yellow
   - 4: Blue
   - 5: Magenta
   - 6: Cyan
   - 7: White
   - 8-15: Bright variants

   Examples: ```ocaml Color.ansi 1 (* Red *) Color.ansi 4 (* Blue *) Color.ansi
   9 (* Bright red *) ```
*)
val ansi256: int -> t

(**
   Create ANSI 256-color palette color (0-255).

   Palette structure:
   - 0-15: Same as basic ANSI colors
   - 16-231: 6×6×6 RGB cube
   - 232-255: Grayscale ramp

   Examples: ```ocaml Color.ansi256 196 (* Bright red *) Color.ansi256 46 (*
   Bright green *) Color.ansi256 240 (* Gray *) ```
*)
val to_string: t -> string

(**
   Convert color to human-readable string.

   Examples: ```ocaml Color.to_string (Color.make "#FF0000") (* "RGB(255,0,0)"
   *) Color.to_string (Color.ansi 4) (* "ANSI(4)" *) Color.to_string
   (Color.ansi256 196) (* "ANSI256(196)" *) Color.to_string Color.no_color (*
   "No_color" *) ```
*)
exception Invalid_color of string

(** Raised when color string format is invalid *)
exception Invalid_color_param of string

(** Raised when color parameter is invalid *)
exception Invalid_color_num of string * int

(** Raised when color number is out of range *)
val no_color: t

(** Disabled color - produces no escape sequence *)
val is_no_color: t -> bool

(**
   Check if color is [no_color].

   Example: ```ocaml Color.is_no_color Color.no_color (* true *)
   Color.is_no_color (Color.ansi 1) (* false *) ```
*)
val is_rgb: t -> bool

(**
   Check if color is RGB type.

   Example: ```ocaml Color.is_rgb (Color.make "#FF0000") (* true *)
   Color.is_rgb (Color.ansi 1) (* false *) ```
*)
val is_ansi: t -> bool

(**
   Check if color is basic ANSI (0-15).

   Example: ```ocaml Color.is_ansi (Color.ansi 4) (* true *) Color.is_ansi
   (Color.ansi256 16) (* false *) ```
*)
val is_ansi256: t -> bool

(**
   Check if color is ANSI256 (0-255).

   Example: ```ocaml Color.is_ansi256 (Color.ansi256 196) (* true *)
   Color.is_ansi256 (Color.ansi 1) (* false *) ```
*)
val to_escape_seq: mode:[`bg | `fg] -> t -> string

(**
   Convert color to ANSI escape sequence parameters.

   The mode determines foreground or background application.
   Returns empty string for [no_color].

   This returns just the parameter part (e.g., "38;2;255;0;0"),
   not the full escape sequence. Use with CSI prefix.

   Examples:
   ```ocaml
   let red = Color.make "#FF0000"
   let fg = Color.to_escape_seq ~mode:`fg red
   (* fg = "38;2;255;0;0" *)

   let bg = Color.to_escape_seq ~mode:`bg red
   (* bg = "48;2;255;0;0" *)

   (* Use in terminal output *)
   Printf.printf "\027[%smRed text\027[0m\n" fg
   ```
*)
