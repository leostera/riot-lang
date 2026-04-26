open Std

type echo_mode =
  | Normal
  | Password
  | None

type t = {
  value: string;
  cursor_pos: int;
  prompt: string;
  placeholder: string;
  width: int;
  char_limit: int;
  echo_mode: echo_mode;
  echo_char: char;
  focused: bool;
  validator: (string -> (unit, string) result) option;
  validation_error: string option;
  offset: int;
  (* Horizontal scroll offset for wide content *)
}

let make = fun () ->
  {
    value = "";
    cursor_pos = 0;
    prompt = "";
    placeholder = "";
    width = 0;
    char_limit = 0;
    echo_mode = Normal;
    echo_char = '*';
    focused = false;
    validator = None;
    validation_error = None;
    offset = 0;
  }

let value = fun t -> t.value

let is_empty = fun t -> t.value = ""

let cursor_position = fun t -> t.cursor_pos

let is_focused = fun t -> t.focused

let is_valid = fun t -> Option.is_none t.validation_error

let validation_error = fun t -> t.validation_error

let validate = fun t ->
  match t.validator with
  | None -> { t with validation_error = None }
  | Some validator -> (
      match validator t.value with
      | Ok () -> { t with validation_error = None }
      | Error msg -> { t with validation_error = Some msg }
    )

let set_value = fun t ~value:str ->
  let value =
    if t.char_limit > 0 && String.length str > t.char_limit then
      String.sub str ~offset:0 ~len:t.char_limit
    else
      str
  in
  let cursor_pos = String.length value in
  validate { t with value; cursor_pos; offset = 0 }

let clear = fun t -> set_value t ~value:""

let set_prompt = fun t ~prompt -> { t with prompt }

let set_placeholder = fun t ~placeholder -> { t with placeholder }

let set_width = fun t ~width -> { t with width = Int.max 0 width }

let set_char_limit = fun t ~limit:char_limit -> { t with char_limit = Int.max 0 char_limit }

let set_echo_mode = fun t ~mode:echo_mode -> { t with echo_mode }

let set_echo_char = fun t ~char:echo_char -> { t with echo_char }

let focus = fun t -> { t with focused = true }

let blur = fun t -> { t with focused = false }

let set_validator = fun t ~validator -> validate { t with validator }

let set_cursor_position = fun t ~pos ->
  let clamped = Int.max 0 (Int.min pos (String.length t.value)) in
  { t with cursor_pos = clamped }

(* Helper: insert text at cursor *)

let insert_at_cursor = fun t text ->
  let before = String.sub t.value ~offset:0 ~len:t.cursor_pos in
  let after = String.sub t.value ~offset:t.cursor_pos ~len:(String.length t.value - t.cursor_pos) in
  let new_value = before ^ text ^ after in
  (* Check char limit *)
  let new_value =
    if t.char_limit > 0 && String.length new_value > t.char_limit then
      before
    else
      new_value
  in
  let new_cursor = Int.min (t.cursor_pos + String.length text) (String.length new_value) in
  validate { t with value = new_value; cursor_pos = new_cursor }

(* Helper: delete character before cursor *)

let delete_char_backward = fun t ->
  if t.cursor_pos = 0 then
    t
  else
    let before = String.sub t.value ~offset:0 ~len:(t.cursor_pos - 1) in
    let after = String.sub t.value ~offset:t.cursor_pos ~len:(String.length t.value - t.cursor_pos) in
    validate { t with value = before ^ after; cursor_pos = t.cursor_pos - 1 }

(* Helper: delete character after cursor *)

let delete_char_forward = fun t ->
  if t.cursor_pos >= String.length t.value then
    t
  else
    let before = String.sub t.value ~offset:0 ~len:t.cursor_pos in
    let after =
      String.sub t.value ~offset:(t.cursor_pos + 1) ~len:(String.length t.value - t.cursor_pos - 1)
    in
    validate { t with value = before ^ after }

(* Helper: clear before cursor *)

let clear_before_cursor = fun t ->
  let after = String.sub t.value ~offset:t.cursor_pos ~len:(String.length t.value - t.cursor_pos) in
  validate { t with value = after; cursor_pos = 0 }

(* Helper: clear after cursor *)

let clear_after_cursor = fun t ->
  let before = String.sub t.value ~offset:0 ~len:t.cursor_pos in
  validate { t with value = before }

(* Helper: delete word backward *)

let delete_word_backward = fun t ->
  if t.cursor_pos = 0 then
    t
  else
    (* Find start of current word *)
    let rec find_word_start pos =
      if pos = 0 then
        0
      else if String.get t.value ~at:(pos - 1) = Some ' ' then
        pos
      else
        find_word_start (pos - 1)
    in
    let word_start = find_word_start t.cursor_pos in
    let before = String.sub t.value ~offset:0 ~len:word_start in
    let after = String.sub t.value ~offset:t.cursor_pos ~len:(String.length t.value - t.cursor_pos) in
    validate { t with value = before ^ after; cursor_pos = word_start }

let handle_paste = fun t text ->
  if not t.focused then
    t
  else
    insert_at_cursor t text

let handle_key = fun t (key: Event.key) modifier ->
  if not t.focused then
    t
  else
    match (key: Event.key) with
    | Event.Left -> set_cursor_position t ~pos:(t.cursor_pos - 1)
    | Event.Right -> set_cursor_position t ~pos:(t.cursor_pos + 1)
    | Event.Home -> set_cursor_position t ~pos:0
    | Event.End -> set_cursor_position t ~pos:(String.length t.value)
    | Event.Backspace when modifier = Event.NoModifier -> delete_char_backward t
    | Event.Delete when modifier = Event.NoModifier -> delete_char_forward t
    | Event.Backspace when modifier = Event.Ctrl || modifier = Event.Alt -> delete_word_backward t
    | Event.Key "u" when modifier = Event.Ctrl -> clear_before_cursor t
    | Event.Key "k" when modifier = Event.Ctrl -> clear_after_cursor t
    | Event.Key "w" when modifier = Event.Ctrl -> delete_word_backward t
    | Event.Key "d" when modifier = Event.Ctrl -> delete_char_forward t
    | Event.Key "h" when modifier = Event.Ctrl -> delete_char_backward t
    | Event.Key "a" when modifier = Event.Ctrl -> set_cursor_position t ~pos:0
    | Event.Key "e" when modifier = Event.Ctrl -> set_cursor_position t ~pos:(String.length t.value)
    | Event.Key "b" when modifier = Event.Ctrl -> set_cursor_position t ~pos:(t.cursor_pos - 1)
    | Event.Key "f" when modifier = Event.Ctrl -> set_cursor_position t ~pos:(t.cursor_pos + 1)
    | Event.Key s when modifier = Event.NoModifier && String.length s = 1 -> insert_at_cursor t s
    | Event.Key s when modifier = Event.Shift && String.length s = 1 -> insert_at_cursor t s
    | Event.Space -> insert_at_cursor t " "
    | _ -> t

let view = fun t ->
  let content =
    if String.length t.value = 0 && not t.focused then
      t.placeholder
    else
      (* Show actual value with echo mode *)
      match t.echo_mode with
      | Normal -> t.value
      | Password -> String.make ~len:(String.length t.value) ~char:t.echo_char
      | None -> ""
  in
  (* Handle width limiting / horizontal scrolling *)
  let visible_content =
    if t.width > 0 && String.length content > t.width then
      let offset =
        if t.cursor_pos < t.offset then
          t.cursor_pos
        else if t.cursor_pos >= t.offset + t.width then
          t.cursor_pos - t.width + 1
        else
          t.offset
      in
      String.sub
        content
        ~offset
        ~len:(Int.min t.width (String.length content - offset))
    else
      content
  in
  (* Add cursor if focused *)
  let with_cursor =
    if t.focused then
      let cursor_visual_pos = Int.min t.cursor_pos (String.length visible_content) in
      if cursor_visual_pos >= String.length visible_content then
        visible_content ^ "█"
      else
        let before = String.sub visible_content ~offset:0 ~len:cursor_visual_pos in
        let at_cursor = String.sub visible_content ~offset:cursor_visual_pos ~len:1 in
        let after =
          String.sub
            visible_content
            ~offset:(cursor_visual_pos + 1)
            ~len:(String.length visible_content - cursor_visual_pos - 1)
        in
        before ^ "\027[7m" ^ at_cursor ^ "\027[0m" ^ after
    else
      visible_content
  in
  t.prompt ^ with_cursor
