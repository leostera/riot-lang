(*
   (** Textarea - Professional multi-line text editor component.

       A feature-rich text area for multi-line text editing with advanced cursor
       navigation, word operations, scrolling, line numbers, and comprehensive
       Emacs-style keybindings.

       ## Example: Basic Text Area}

       ```ocaml
         open Std
         open Minttea

         type model = { editor : Textarea.t }

         let init () =
           let editor = Textarea.make ()
             |> Textarea.set_width 80
             |> Textarea.set_height 20
             |> Textarea.set_placeholder "Enter your code here..."
             |> Textarea.set_show_line_numbers true
             |> Textarea.focus
           in
           ({ editor }, Command.Noop)

         let update event model =
           match event with
           | Event.KeyDown (key, mods) ->
               let editor = Textarea.handle_key model.editor key mods in
               ({ editor }, Command.Noop)
           | Event.Paste text ->
               let editor = Textarea.insert_string model.editor text in
               ({ editor }, Command.Noop)
           | _ -> (model, Command.Noop)

         let view model =
           Textarea.view model.editor ^ "\n\n" ^
           format "Line %d, Col %d | %d lines"
             (fst (Textarea.cursor_position model.editor) + 1)
             (snd (Textarea.cursor_position model.editor) + 1)
             (Textarea.line_count model.editor)
       ```

       ## Example: Code Editor with Syntax}

       ```ocaml
         let editor = Textarea.make ()
           |> Textarea.set_show_line_numbers true
           |> Textarea.set_prompt ""
           |> Textarea.set_width 100
           |> Textarea.set_height 30
           |> Textarea.set_char_limit 100000
       ``` *)

   open Std

   (** ## Types} *)

   type t
   (** A textarea instance *)

   (** ## Creation} *)

   val make : unit -> t
   (** [make ()] creates a new empty textarea.

       Defaults:
       - width: 40 columns
       - height: 6 rows
       - cursor at 0,0
       - focused: false
       - line numbers: false
       - single empty line *)

   (** ## Content Access} *)

   val value : t -> string
   (** [value textarea] returns the full text content with newlines. *)

   val lines : t -> string list
   (** [lines textarea] returns content as list of lines. *)

   val line_count : t -> int
   (** [line_count textarea] returns the number of lines. *)

   val current_line : t -> string
   (** [current_line textarea] returns the text of the line at cursor. *)

   val is_empty : t -> bool
   (** [is_empty textarea] returns true if no content (only empty line). *)

   val length : t -> int
   (** [length textarea] returns total character count (including newlines). *)

   (** ## Content Modification} *)

   val set_value : t -> value:string -> t
   (** [set_value textarea text] replaces all content. Moves cursor to end. *)

   val insert_string : t -> string -> t
   (** [insert_string textarea text] inserts text at cursor position.

       Handles newlines properly, splitting into multiple lines. *)

   val insert_char : t -> char -> t
   (** [insert_char textarea c] inserts a single character at cursor. *)

   val insert_newline : t -> t
   (** [insert_newline textarea] inserts a newline, splitting current line. *)

   val clear : t -> t
   (** [clear textarea] removes all text, leaves single empty line. *)

   val reset : t -> t
   (** [reset textarea] resets to initial state (clear + cursor to 0,0). *)

   (** ## Display Configuration} *)

   val set_width : t -> width:int -> t
   (** [set_width textarea w] sets the display width in columns. *)

   val set_height : t -> height:int -> t
   (** [set_height textarea h] sets the visible height in rows. *)

   val set_placeholder : t -> placeholder:string -> t
   (** [set_placeholder textarea text] sets text shown when empty and unfocused. *)

   val set_prompt : t -> prompt:string -> t
   (** [set_prompt textarea text] sets prefix shown on each line (e.g. "> ").

       Set to "" to disable. *)

   val set_show_line_numbers : t -> show:bool -> t
   (** [set_show_line_numbers textarea show] enables/disables line numbers. *)

   val set_end_of_buffer_char : t -> char:char -> t
   (** [set_end_of_buffer_char textarea c] sets char shown on empty lines at end.

       Set to ' ' to hide. Default: '~' *)

   val set_char_limit : t -> limit:int -> t
   (** [set_char_limit textarea limit] sets max total characters. 0 = unlimited. *)

   val set_max_height : t -> max_height:int -> t
   (** [set_max_height textarea h] sets maximum height. 0 = unlimited. *)

   val set_max_width : t -> max_width:int -> t
   (** [set_max_width textarea w] sets maximum width. 0 = unlimited. *)

   (** ## Cursor Position} *)

   val cursor_position : t -> int * int
   (** [cursor_position textarea] returns (row, col) of cursor (0-based). *)

   val set_cursor : t -> pos:int -> t
   (** [set_cursor textarea col] sets cursor column on current row (clamped). *)

   val cursor_start : t -> t
   (** [cursor_start textarea] moves cursor to start of current line. *)

   val cursor_end : t -> t
   (** [cursor_end textarea] moves cursor to end of current line. *)

   (** ## Cursor Movement} *)

   val move_up : t -> t
   (** [move_up textarea] moves cursor up one line. *)

   val move_down : t -> t
   (** [move_down textarea] moves cursor down one line. *)

   val move_left : t -> t
   (** [move_left textarea] moves cursor left one character (wraps to prev line). *)

   val move_right : t -> t
   (** [move_right textarea] moves cursor right one character (wraps to next line). *)

   val word_left : t -> t
   (** [word_left textarea] moves cursor to start of previous word. *)

   val word_right : t -> t
   (** [word_right textarea] moves cursor to start of next word. *)

   val goto_start : t -> t
   (** [goto_start textarea] moves cursor to start of document (0,0). *)

   val goto_end : t -> t
   (** [goto_end textarea] moves cursor to end of document. *)

   (** ## Deletion} *)

   val delete_char_before : t -> t
   (** [delete_char_before textarea] deletes character before cursor (backspace). *)

   val delete_char_after : t -> t
   (** [delete_char_after textarea] deletes character after cursor (delete). *)

   val delete_before_cursor : t -> t
   (** [delete_before_cursor textarea] deletes all text from line start to cursor. *)

   val delete_after_cursor : t -> t
   (** [delete_after_cursor textarea] deletes all text from cursor to line end. *)

   val delete_word_left : t -> t
   (** [delete_word_left textarea] deletes word before cursor. *)

   val delete_word_right : t -> t
   (** [delete_word_right textarea] deletes word after cursor. *)

   (** ## Advanced Text Operations} *)

   val transpose_left : t -> t
   (** [transpose_left textarea] swaps character at cursor with previous char. *)

   val uppercase_word_right : t -> t
   (** [uppercase_word_right textarea] converts next word to uppercase. *)

   val lowercase_word_right : t -> t
   (** [lowercase_word_right textarea] converts next word to lowercase. *)

   val capitalize_word_right : t -> t
   (** [capitalize_word_right textarea] capitalizes first letter of next word. *)

   (** ## Focus} *)

   val focus : t -> t
   (** [focus textarea] enables editing and shows cursor. *)

   val blur : t -> t
   (** [blur textarea] disables editing and hides cursor. *)

   val is_focused : t -> bool
   (** [is_focused textarea] returns true if focused. *)

   (** ## Input Handling} *)

   val handle_key : t -> Event.key -> Event.modifier -> t
   (** [handle_key textarea key modifier] processes keyboard input.

       {b Navigation:}
       - Up/Ctrl+P: move up
       - Down/Ctrl+N: move down
       - Left/Ctrl+B: move left
       - Right/Ctrl+F: move right
       - Alt+B/Alt+Left: word left
       - Alt+F/Alt+Right: word right
       - Home/Ctrl+A: line start
       - End/Ctrl+E: line end
       - Ctrl+Home/Alt+<: document start
       - Ctrl+End/Alt+>: document end

       {b Editing:}
       - Enter/Ctrl+M: insert newline
       - Backspace/Ctrl+H: delete char before
       - Delete/Ctrl+D: delete char after
       - Ctrl+K: delete to line end
       - Ctrl+U: delete to line start
       - Ctrl+W/Alt+Backspace: delete word left
       - Alt+D/Alt+Delete: delete word right

       {b Advanced:}
       - Ctrl+T: transpose characters
       - Alt+U: uppercase word
       - Alt+L: lowercase word
       - Alt+C: capitalize word

       Returns updated textarea. No-op if not focused. *)

   (** ## Rendering} *)

   val view : t -> string
   (** [view textarea] renders the textarea for display.

       Features:
       - Line numbers (if enabled)
       - Prompt on each line
       - Scrolling viewport
       - Cursor rendering
       - End of buffer markers (~)
       - Placeholder text (when empty and unfocused)

       The viewport automatically scrolls to keep cursor visible. *)
*)
