(*
   open Std
   open Std.Collections

   (* Internal representation using char lists for easy editing *)
   type t = {
     lines : char list Vector.t;
     row : int;
     col : int;
     last_char_offset : int;
     width : int;
     height : int;
     show_line_numbers : bool;
     prompt : string;
     placeholder : string;
     end_of_buffer_char : char;
     char_limit : int;
     max_height : int;
     max_width : int;
     focused : bool;
     viewport_top : int;
   }

   let make () =
     {
       lines = Vector.from_list [[]];
       row = 0;
       col = 0;
       last_char_offset = 0;
       width = 40;
       height = 6;
       show_line_numbers = false;
       prompt = "";
       placeholder = "";
       end_of_buffer_char = '~';
       char_limit = 0;
       max_height = 0;
       max_width = 0;
       focused = false;
       viewport_top = 0;
     }

   (* Helper: get line at row *)
   let get_line t row =
     match Vector.get t.lines row with
     | Some line -> line
     | None -> []

   (* Helper: set line at row *)
   let set_line t row line =
     Vector.set t.lines row line;
     t

   (* Helper: char list to string *)
   let chars_to_string chars = String.from_seq (List.to_seq chars)

   (* Helper: string to char list *)
   let string_to_chars s = List.from_seq (String.to_seq s)

   (* Content access *)
   let lines t =
     List.init (Vector.length t.lines) (fun i ->
       chars_to_string (get_line t i)
     )

   let value t = String.concat "\n" (lines t)

   let line_count t = Vector.length t.lines

   let current_line t = chars_to_string (get_line t t.row)

   let is_empty t =
     Vector.length t.lines = 1 && List.length (get_line t 0) = 0

   let length t =
     let line_lengths = Vector.fold_left (fun acc line ->
       acc + List.length line
     ) 0 t.lines in
     line_lengths + (Vector.length t.lines - 1)  (* Add newlines *)

   (* Cursor helpers *)
   let clamp_col t row col =
     let line = get_line t row in
     max 0 (min col (List.length line))

   let clamp_row t row =
     max 0 (min row (Vector.length t.lines - 1))

   let set_cursor t ~pos:col =
     let col = clamp_col t t.row col in
     { t with col; last_char_offset = col }

   let cursor_start t = set_cursor t 0

   let cursor_end t =
     let line = get_line t t.row in
     set_cursor t (List.length line)

   let cursor_position t = (t.row, t.col)

   (* Movement *)
   let move_up t =
     if t.row > 0 then
       let new_row = t.row - 1 in
       let new_col = clamp_col t new_row t.last_char_offset in
       { t with row = new_row; col = new_col }
     else t

   let move_down t =
     if t.row < Vector.length t.lines - 1 then
       let new_row = t.row + 1 in
       let new_col = clamp_col t new_row t.last_char_offset in
       { t with row = new_row; col = new_col }
     else t

   let move_left t =
     if t.col > 0 then
       set_cursor t (t.col - 1)
     else if t.row > 0 then
       let new_row = t.row - 1 in
       let line = get_line t new_row in
       { t with row = new_row; col = List.length line; last_char_offset = List.length line }
     else t

   let move_right t =
     let line = get_line t t.row in
     if t.col < List.length line then
       set_cursor t (t.col + 1)
     else if t.row < Vector.length t.lines - 1 then
       { t with row = t.row + 1; col = 0; last_char_offset = 0 }
     else t

   (* Word movement helpers *)
   let is_word_char c =
     (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
     (c >= '0' && c <= '9') || c = '_'

   let is_whitespace c = c = ' ' || c = '\t'

   let word_left t =
     let rec go t =
       let line = get_line t t.row in
       if t.col > 0 then
         let c = List.nth line (t.col - 1) in
         if is_whitespace c then
           go (move_left t)
         else
           t
       else if t.row > 0 then
         go (move_left t)
       else
         t
     in
     let t = go t in
     let rec skip_word t =
       let line = get_line t t.row in
       if t.col > 0 then
         let c = List.nth line (t.col - 1) in
         if is_word_char c then
           skip_word (move_left t)
         else
           t
       else if t.row > 0 then
         skip_word (move_left t)
       else
         t
     in
     skip_word t

   let word_right t =
     let rec skip_whitespace t =
       let line = get_line t t.row in
       if t.col < List.length line then
         let c = List.nth line t.col in
         if is_whitespace c then
           skip_whitespace (move_right t)
         else
           t
       else if t.row < Vector.length t.lines - 1 then
         skip_whitespace (move_right t)
       else
         t
     in
     let t = skip_whitespace t in
     let rec skip_word t =
       let line = get_line t t.row in
       if t.col < List.length line then
         let c = List.nth line t.col in
         if is_word_char c then
           skip_word (move_right t)
         else
           t
       else if t.row < Vector.length t.lines - 1 then
         skip_word (move_right t)
       else
         t
     in
     skip_word t

   let goto_start t =
     { t with row = 0; col = 0; last_char_offset = 0 }

   let goto_end t =
     let last_row = Vector.length t.lines - 1 in
     let line = get_line t last_row in
     { t with row = last_row; col = List.length line; last_char_offset = List.length line }

   (* Insertion *)
   let insert_char t c =
     if t.char_limit > 0 && length t >= t.char_limit then t
     else
       let line = get_line t t.row in
       let before = List.take t.col line in
       let after = List.drop t.col line in
       let new_line = before @ [c] @ after in
       set_line t t.row new_line |> set_cursor (t.col + 1)

   let split_line t row col =
     if t.max_height > 0 && Vector.length t.lines >= t.max_height then t
     else
       let line = get_line t row in
       let before = List.take col line in
       let after = List.drop col line in
       Vector.set t.lines row before;
       Vector.insert t.lines (row + 1) after;
       { t with row = row + 1; col = 0; last_char_offset = 0 }

   let insert_newline t =
     split_line t t.row t.col

   let insert_string t s =
     let chars = string_to_chars s in
     let rec insert_chars t = function
       | [] -> t
       | '\n' :: rest ->
           let t = insert_newline t in
           insert_chars t rest
       | c :: rest ->
           let t = insert_char t c in
           insert_chars t rest
     in
     insert_chars t chars

   let set_value t ~value:s =
     let t = { t with lines = Vector.from_list [[]]; row = 0; col = 0; last_char_offset = 0 } in
     insert_string t s

   let clear t =
     { t with lines = Vector.from_list [[]]; row = 0; col = 0; last_char_offset = 0 }

   let reset t = clear t

   (* Deletion *)
   let merge_line_above t row =
     if row <= 0 then t
     else
       let current_line = get_line t row in
       let prev_line = get_line t (row - 1) in
       let col = List.length prev_line in
       Vector.set t.lines (row - 1) (prev_line @ current_line);
       Vector.remove t.lines row;
       { t with row = row - 1; col; last_char_offset = col }

   let merge_line_below t row =
     if row >= Vector.length t.lines - 1 then t
     else
       let current_line = get_line t row in
       let next_line = get_line t (row + 1) in
       Vector.set t.lines row (current_line @ next_line);
       Vector.remove t.lines (row + 1);
       t

   let delete_char_before t =
     if t.col > 0 then
       let line = get_line t t.row in
       let new_line = List.take (t.col - 1) line @ List.drop t.col line in
       set_line t t.row new_line |> set_cursor (t.col - 1)
     else
       merge_line_above t t.row

   let delete_char_after t =
     let line = get_line t t.row in
     if t.col < List.length line then
       let new_line = List.take t.col line @ List.drop (t.col + 1) line in
       set_line t t.row new_line
     else
       merge_line_below t t.row

   let delete_before_cursor t =
     let line = get_line t t.row in
     let new_line = List.drop t.col line in
     set_line t t.row new_line |> set_cursor 0

   let delete_after_cursor t =
     let line = get_line t t.row in
     let new_line = List.take t.col line in
     set_line t t.row new_line

   let delete_word_left t =
     let start_col = t.col in
     let t = word_left t in
     let end_col = t.col in
     if t.row = t.row then  (* Same line *)
       let line = get_line t t.row in
       let new_line = List.take end_col line @ List.drop start_col line in
       set_line t t.row new_line
     else
       t  (* Cross-line deletion not implemented for simplicity *)

   let delete_word_right t =
     let start_col = t.col in
     let saved_row = t.row in
     let t_moved = word_right t in
     let end_col = t_moved.col in
     if saved_row = t_moved.row then  (* Same line *)
       let line = get_line t saved_row in
       let new_line = List.take start_col line @ List.drop end_col line in
       set_line t saved_row new_line
     else
       t  (* Cross-line deletion not implemented for simplicity *)

   (* Advanced operations *)
   let transpose_left t =
     let line = get_line t t.row in
     let len = List.length line in
     if t.col = 0 || len < 2 then t
     else
       let col = if t.col >= len then len - 1 else t.col in
       let before = List.take (col - 1) line in
       let c1 = List.nth line (col - 1) in
       let c2 = List.nth line col in
       let after = List.drop (col + 1) line in
       let new_line = before @ [c2; c1] @ after in
       let new_col = if t.col < len then col + 1 else col in
       set_line t t.row new_line |> set_cursor new_col

   let transform_word_right t transform =
     let start_col = t.col in
     let start_row = t.row in
     let t_moved = word_right t in
     if start_row = t_moved.row then
       let line = get_line t start_row in
       let word_chars = List.take t_moved.col line |> List.drop start_col in
       let transformed = List.map transform word_chars in
       let new_line = List.take start_col line @ transformed @ List.drop t_moved.col line in
       set_line t start_row new_line |> fun t -> { t with col = t_moved.col }
     else
       t

   let uppercase_word_right t =
     transform_word_right t Char.uppercase_ascii

   let lowercase_word_right t =
     transform_word_right t Char.lowercase_ascii

   let capitalize_word_right t =
     let start_col = t.col in
     let start_row = t.row in
     let t_moved = word_right t in
     if start_row = t_moved.row && t_moved.col > start_col then
       let line = get_line t start_row in
       let word_chars = List.take t_moved.col line |> List.drop start_col in
       let transformed = match word_chars with
         | [] -> []
         | first :: rest -> Char.uppercase_ascii first :: List.map Char.lowercase_ascii rest
       in
       let new_line = List.take start_col line @ transformed @ List.drop t_moved.col line in
       set_line t start_row new_line |> fun t -> { t with col = t_moved.col }
     else
       t

   (* Configuration *)
   let set_width t ~width:w = { t with width = max 1 w }
   let set_height t ~height:h = { t with height = max 1 h }
   let set_placeholder t ~placeholder:p = { t with placeholder = p }
   let set_prompt t ~prompt:p = { t with prompt = p }
   let set_show_line_numbers t ~show = { t with show_line_numbers = show }
   let set_end_of_buffer_char t ~char:c = { t with end_of_buffer_char = c }
   let set_char_limit t ~limit = { t with char_limit = max 0 limit }
   let set_max_height t ~max_height:h = { t with max_height = max 0 h }
   let set_max_width t ~max_width:w = { t with max_width = max 0 w }

   (* Focus *)
   let focus t = { t with focused = true }
   let blur t = { t with focused = false }
   let is_focused t = t.focused

   (* Viewport management *)
   let reposition_viewport t =
     let visible_height = t.height in
     let cursor_row = t.row in

     (* Ensure cursor is visible *)
     let new_top =
       if cursor_row < t.viewport_top then
         cursor_row
       else if cursor_row >= t.viewport_top + visible_height then
         cursor_row - visible_height + 1
       else
         t.viewport_top
     in
     { t with viewport_top = max 0 new_top }

   (* Input handling *)
   let handle_key t (key : Event.key) modifier =
     if not t.focused then t
     else
       let open Event in
       let t = match (key : Event.key), modifier with
         (* Navigation *)
         | Up, NoModifier | Key "p", Ctrl -> move_up t
         | Down, NoModifier | Key "n", Ctrl -> move_down t
         | Left, NoModifier | Key "b", Ctrl -> move_left t
         | Right, NoModifier | Key "f", Ctrl -> move_right t
         | Left, Alt | Key "b", Alt -> word_left t
         | Right, Alt | Key "f", Alt -> word_right t
         | Home, NoModifier | Key "a", Ctrl -> cursor_start t
         | End, NoModifier | Key "e", Ctrl -> cursor_end t
         | Home, Ctrl -> goto_start t
         | End, Ctrl -> goto_end t

         (* Editing *)
         | Enter, NoModifier | Key "m", Ctrl -> insert_newline t
         | Backspace, NoModifier | Key "h", Ctrl -> delete_char_before t
         | Delete, NoModifier | Key "d", Ctrl -> delete_char_after t
         | Key "k", Ctrl -> delete_after_cursor t
         | Key "u", Ctrl -> delete_before_cursor t
         | Backspace, Alt | Key "w", Ctrl -> delete_word_left t
         | Delete, Alt | Key "d", Alt -> delete_word_right t

         (* Advanced *)
         | Key "t", Ctrl -> transpose_left t
         | Key "u", Alt -> uppercase_word_right t
         | Key "l", Alt -> lowercase_word_right t
         | Key "c", Alt -> capitalize_word_right t

         (* Character input *)
         | Key s, NoModifier when String.length s = 1 ->
             insert_char t s.[0]
         | Key s, Shift when String.length s = 1 ->
             insert_char t s.[0]
         | Space, _ -> insert_char t ' '
         | Tab, _ -> insert_char t '\t'

         | _ -> t
       in
       reposition_viewport t

   (* Rendering *)
   let view t =
     let module B = Buffer in
     let buf = B.create 256 in

     (* Show placeholder if empty and unfocused *)
     if is_empty t && not t.focused && t.placeholder <> "" then begin
       B.add_string buf t.placeholder;
       B.contents buf
     end else begin
       let total_lines = Vector.length t.lines in
       let visible_start = t.viewport_top in
       let visible_end = min total_lines (visible_start + t.height) in

       (* Calculate line number width *)
       let ln_width = if t.show_line_numbers then
         String.length (string_of_int total_lines) + 1
       else 0 in

       for i = visible_start to visible_end - 1 do
         if i > visible_start then B.add_char buf '\n';

         (* Line number *)
         if t.show_line_numbers then begin
           let ln_str = string_of_int (i + 1) in
           let padding = ln_width - String.length ln_str in
           B.add_string buf (String.make padding ' ');
           B.add_string buf ln_str;
           B.add_char buf ' '
         end;

         (* Prompt *)
         B.add_string buf t.prompt;

         (* Line content *)
         let line = get_line t i in
         B.add_string buf (chars_to_string line);

         (* Cursor *)
         if t.focused && i = t.row then begin
           if t.col = List.length line then
             B.add_string buf " "  (* Cursor at end *)
         end
       done;

       (* End of buffer markers *)
       let lines_shown = visible_end - visible_start in
       if lines_shown < t.height && t.end_of_buffer_char <> ' ' then begin
         for _ = lines_shown to t.height - 1 do
           B.add_char buf '\n';
           if t.show_line_numbers then
             B.add_string buf (String.make (ln_width + 1) ' ');
           B.add_string buf t.prompt;
           B.add_char buf t.end_of_buffer_char
         done
       end;

       B.contents buf
     end
*)
