(**
   Single-line text editor component.

   Provides a text input field for forms, search boxes, command input, and any
   single-line text entry. Supports cursor movement, editing, validation,
   password masking, and placeholder text.

   ## Example: Basic Text Input

   ```ocaml
   open Std
   open Minttea

   type model = { input : Textinput.t }

   let init () =
     let input = Textinput.make ()
       |> Textinput.set_placeholder "Enter your name..."
       |> Textinput.set_width 40
       |> Textinput.focus
     in
     ({ input }, Command.Noop)

   let update event model =
     match event with
     | Event.KeyDown (Enter, _) ->
         let value = Textinput.value model.input in
         (* Process the input value *)
         (model, Command.Quit)
     | Event.KeyDown (key, mods) ->
         let input = Textinput.handle_key model.input key mods in
         ({ input }, Command.Noop)
     | _ -> (model, Command.Noop)

   let view model =
     Textinput.view model.input
   ```

   ## Example: Password Input

   ```ocaml
   let input = Textinput.make ()
     |> Textinput.set_echo_mode Password
     |> Textinput.set_echo_char '*'
     |> Textinput.set_placeholder "Enter password"
   ```
*)
open Std

(** A text input instance. *)
type t
type echo_mode =
  | Normal
  (* Display text as-is *)
  | Password
  (* Mask with echo character *)
  | None

val make: unit -> t

(**
   `make ()` creates a new empty text input.

   Defaults:
   - width: unlimited
   - char_limit: unlimited
   - cursor at position 0
   - focused: false
   - echo_mode: Normal
*)
val value: t -> string

(** `value input` returns the current input value. *)
val set_value: t -> value:string -> t

(** `set_value input str` sets the input value and moves cursor to end. *)
val clear: t -> t

(** `clear input` clears all text (equivalent to `set_value input ""`). *)
val is_empty: t -> bool

(** `is_empty input` returns true if value is empty string. *)
val set_prompt: t -> prompt:string -> t

(** `set_prompt input prompt` sets the text shown before the input (e.g. "> "). *)
val set_placeholder: t -> placeholder:string -> t

(** `set_placeholder input text` sets the text shown when input is empty. *)
val set_width: t -> width:int -> t

(**
   `set_width input width` sets the maximum display width.

   Input acts as a horizontally scrolling viewport if text exceeds width.
   Set to 0 for unlimited width.
*)
val set_char_limit: t -> limit:int -> t

(**
   `set_char_limit input limit` sets the maximum number of characters.

   Prevents typing beyond this limit. Set to 0 for unlimited.
*)
val set_echo_mode: t -> mode:echo_mode -> t

(**
   `set_echo_mode input mode` controls how text is displayed.

   - Normal: show actual text
   - Password: show echo character for each char
   - None: show nothing (useful for hidden password entry)
*)
val set_echo_char: t -> char:char -> t

(** `set_echo_char input c` sets the character used in Password mode (default: '*'). *)
val focus: t -> t

(** `focus input` gives focus to the input (enables editing, shows cursor). *)
val blur: t -> t

(** `blur input` removes focus (disables editing, hides cursor). *)
val is_focused: t -> bool

(** `is_focused input` returns true if input has focus. *)
val cursor_position: t -> int

(** `cursor_position input` returns the cursor position (0-based index). *)
val set_cursor_position: t -> pos:int -> t

(** `set_cursor_position input pos` moves cursor to position (clamped to valid range). *)
val set_validator: t -> validator:(string -> (unit, string) result) option -> t

(**
   `set_validator input validator` sets an optional validation function.

   The validator is called after each edit. If it returns `Error msg`,
   the input is marked as invalid (but still editable).
*)
val is_valid: t -> bool

(** `is_valid input` returns true if validation passed or no validator is set. *)
val validation_error: t -> string option

(** `validation_error input` returns the validation error message, if any. *)
val handle_key: t -> Event.key -> Event.modifier -> t

(**
   `handle_key input key modifier` processes a keyboard event.

   Standard bindings:
   - Left/Right, Ctrl+B/F: move cursor
   - Home/Ctrl+A, End/Ctrl+E: start/end of line
   - Backspace/Ctrl+H: delete char before cursor
   - Delete/Ctrl+D: delete char after cursor
   - Ctrl+U: clear before cursor
   - Ctrl+K: clear after cursor
   - Ctrl+W: delete word backward
   - Character keys: insert at cursor

   Returns updated input. No-op if input is not focused.
*)
val handle_paste: t -> string -> t

(**
   `handle_paste input text` inserts pasted text at cursor position.

   Respects char_limit. Only works if focused.
*)
val view: t -> string

(**
   `view input` renders the text input for display.

   Format: `[prompt][visible_text][cursor]`

   - Shows placeholder if empty and not focused
   - Applies echo mode (password masking, etc.)
   - Handles horizontal scrolling if width is set
   - Shows/hides cursor based on focus
*)
