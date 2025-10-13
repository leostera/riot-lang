(** Text styling with colors and attributes.

    Provides a composable way to create styled text for terminal output. Styles
    can be built by chaining attribute functions and then applied to strings.

    ## Examples

    Basic styling: ```ocaml let red_bold = Style.default |> Style.fg (Color.make
    "#FF0000") |> Style.bold

    print_endline (Style.styled red_bold "Error!") ```

    Predefined styles: ```ocaml let error_style = Style.default |> Style.fg
    (Color.make "#FF0000") |> Style.bold

    let warning_style = Style.default |> Style.fg (Color.make "#FFA500")

    let success_style = Style.default |> Style.fg (Color.make "#00FF00")

    let info_style = Style.default |> Style.fg (Color.make "#00FFFF")

    (* Use in application *) print_endline (Style.styled error_style "❌
    Operation failed"); print_endline (Style.styled warning_style "⚠️ Be
    careful"); print_endline (Style.styled success_style "✓ Done");
    print_endline (Style.styled info_style "ℹ Processing..."); ```

    Complex styling: ```ocaml let fancy = Style.default |> Style.fg (Color.make
    "#FF00FF") |> Style.bg (Color.make "#000000") |> Style.bold |> Style.italic
    |> Style.underline

    print_endline (Style.styled fancy "Fancy text!") ```

    Combining styles: ```ocaml let base = Style.default |> Style.bold in

    let red_bold = base |> Style.fg (Color.make "#FF0000") in let blue_bold =
    base |> Style.fg (Color.make "#0000FF") in

    Printf.printf "%s and %s\n" (Style.styled red_bold "Red") (Style.styled
    blue_bold "Blue") ``` *)

type t = {
  fg : Color.t option;  (** Foreground (text) color *)
  bg : Color.t option;  (** Background color *)
  bold : bool;  (** Bold/bright text *)
  faint : bool;  (** Faint/dim text *)
  italic : bool;  (** Italic text *)
  underline : bool;  (** Underlined text *)
  blink : bool;  (** Blinking text *)
  reverse : bool;  (** Reverse video (swap fg/bg) *)
  strikethrough : bool;  (** Strikethrough/crossed-out text *)
  overline : bool;  (** Overlined text *)
}

val default : t
(** Default style with no attributes.

    All boolean flags are [false], colors are [None].

    Example: ```ocaml let plain_text = Style.styled Style.default "Hello" (*
    Same as "Hello" - no styling applied *) ``` *)

val fg : Color.t -> t -> t
(** Set foreground (text) color.

    Examples: ```ocaml Style.default |> Style.fg (Color.make "#FF0000") (* Red
    text *) Style.default |> Style.fg (Color.ansi 4) (* Blue text *) ``` *)

val bg : Color.t -> t -> t
(** Set background color.

    Examples: ```ocaml Style.default |> Style.bg (Color.make "#000000") (* Black
    background *) Style.default |> Style.bg (Color.ansi 7) (* White background
    *) ``` *)

val bold : t -> t
(** Make text bold/bright.

    Example: ```ocaml let bold_red = Style.default |> Style.fg (Color.ansi 1) |>
    Style.bold ``` *)

val faint : t -> t
(** Make text faint/dim.

    Not widely supported in all terminals.

    Example: ```ocaml let dimmed = Style.default |> Style.faint ``` *)

val italic : t -> t
(** Make text italic.

    Example: ```ocaml let emphasis = Style.default |> Style.italic ``` *)

val underline : t -> t
(** Underline text.

    Example: ```ocaml let underlined = Style.default |> Style.underline ``` *)

val blink : t -> t
(** Make text blink.

    Not recommended for accessibility. Limited terminal support.

    Example: ```ocaml let blinking = Style.default |> Style.blink ``` *)

val reverse : t -> t
(** Reverse video - swap foreground and background colors.

    Example: ```ocaml let highlighted = Style.default |> Style.reverse ``` *)

val strikethrough : t -> t
(** Strike through text (crossed out).

    Example: ```ocaml let deleted = Style.default |> Style.strikethrough
    print_endline (Style.styled deleted "Deprecated function") ``` *)

val overline : t -> t
(** Draw line over text.

    Limited terminal support.

    Example: ```ocaml let overlined = Style.default |> Style.overline ``` *)

val to_escape_seq : t -> string
(** Convert style to ANSI escape sequence parameters.
    
    Returns the SGR (Select Graphic Rendition) parameter string
    without the CSI prefix or 'm' suffix.
    
    Example:
    ```ocaml
    let style = Style.default |> Style.bold |> Style.fg (Color.ansi 1)
    let seq = Style.to_escape_seq style
    (* seq might be "1;31" (bold + red) *)
    
    Printf.printf "\027[%sm%s\027[0m\n" seq "Styled text"
    ```
    
    Note: Usually you want [styled] instead, which handles escapes for you. *)

val styled : t -> string -> string
(** Apply style to a string.

    Wraps the string in ANSI escape sequences to apply the style, and resets to
    default at the end.

    Examples: ```ocaml let red = Style.default |> Style.fg (Color.make
    "#FF0000") print_endline (Style.styled red "Error message")

    (* Multiple styled strings *) Printf.printf "%s %s %s\n" (Style.styled
    (Style.default |> Style.bold) "Bold") (Style.styled (Style.default |>
    Style.italic) "Italic") (Style.styled (Style.default |> Style.underline)
    "Underline")

    (* Nested styling works *) let outer = Style.default |> Style.bold let inner
    = Style.default |> Style.fg (Color.ansi 1) Printf.printf "%s\n"
    (Style.styled outer ("Bold " ^ Style.styled inner "red" ^ " bold")) ```

    If style is [default] (no attributes), returns string unchanged. *)
