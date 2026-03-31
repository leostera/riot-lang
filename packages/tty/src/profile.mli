(** Terminal color capability detection and conversion.

    This module detects the color capabilities of the current terminal and
    provides automatic color conversion to match those capabilities. It handles
    terminals ranging from no color support to full truecolor RGB.

    {1 Example: Automatic Color Adaptation}

    {[
      open Std
      open Tty

      let () =
        let profile = Profile.from_env () in

        (* Create a truecolor *)
        let orange = Color.rgb 255 128 0 in

        (* Automatically convert to terminal's capability *)
        let terminal_color = Profile.convert profile orange in

        (* This will be truecolor, ANSI256, ANSI, or no color
           depending on terminal capability *)
        let style = Style.fg terminal_color in
        println (Style.render style "Adaptive orange text")
    ]}

    {1 Example: Explicit Profile Selection}

    {[
      let render_for_basic_terminal text =
        let profile = Profile.default in
        (* Conservative default *)
        let color = Color.rgb 255 0 0 in
        let adapted = Profile.convert profile color in
        Style.render (Style.fg adapted) text

      let render_for_truecolor text =
        let profile = Profile.from_env () in
        (* Detect capability *)
        let color = Color.rgb 123 234 45 in
        let adapted = Profile.convert profile color in
        Style.render (Style.fg adapted) text
    ]}

    {1 Color Degradation Strategy}

    When converting colors to less capable terminals, the profile uses
    intelligent degradation:

    - {b Truecolor → ANSI256}: Maps to closest 256-color palette entry
    - {b ANSI256 → ANSI}: Maps to closest 16-color palette entry
    - {b ANSI → No Color}: Removes all color information

    {1 Environment Detection}

    {!from_env} checks these environment variables in order:
    - [COLORTERM=truecolor] or [COLORTERM=24bit] → Truecolor support
    - [TERM] contains "256color" → 256-color support
    - [TERM] contains "color" → Basic ANSI color support
    - Otherwise → No color support *)
(** Terminal color profile representing color capability level *)
(** [from_env ()] detects the terminal's color capability from environment
    variables like [COLORTERM] and [TERM].

    Returns a profile that matches the detected capability. *)
type t
val from_env : unit -> t

(** [default] provides a conservative profile for basic terminals.

    Assumes 16-color ANSI support, which is widely compatible. *)
val default : t

(** [convert profile color] adapts [color] to match the [profile]'s capability.

    If the color is already compatible (e.g., ANSI color on ANSI profile), it
    returns the color unchanged. Otherwise, it converts to the closest
    equivalent color the terminal can display. *)
val convert : t -> Color.t -> Color.t
