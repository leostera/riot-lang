open Std
open Std.IO

type 'a t = {
  all_items: 'a list;
  filtered_items: 'a list;
  selected: int;  (* Index in filtered_items *)
  height: int;  (* 0 = unlimited *)
  width: int;  (* 0 = unlimited *)
  cursor_char: string;
  filter_enabled: bool;
  filter_query: string;
  filtering_active: bool;  (* User is typing filter *)
  render: 'a -> string;
  scroll_offset: int;  (* For pagination when height is limited *)
}

let default_render = fun _x ->
  (* Default render that cannot know about the type *)
  "<item>"

let make = fun ?(render = default_render) items ->
  {
    all_items = items;
    filtered_items = items;
    selected = 0;
    height = 0;
    width = 0;
    cursor_char = "> ";
    filter_enabled = true;
    filter_query = "";
    filtering_active = false;
    render;
    scroll_offset = 0;
  }

let set_height = fun t ~height:h -> { t with height = max 0 h }

let set_width = fun t ~width:w -> { t with width = max 0 w }

let set_cursor_char = fun t ~char:c -> { t with cursor_char = c }

let set_filter_enabled = fun t ~enabled -> { t with filter_enabled = enabled }

let items = fun t -> t.all_items

let visible_items = fun t -> t.filtered_items

let filter_query = fun t -> t.filter_query

let is_filtering = fun t -> t.filtering_active

let selected_item = fun t ->
  if List.length t.filtered_items = 0 then
    None
  else
    match List.get t.filtered_items ~at:t.selected with
    | Some item -> Some item
    | None -> None

let selected_index = fun t ->
  if List.length t.filtered_items = 0 then
    None
  else
    Some t.selected

let clamp_selection = fun t ->
  let len = List.length t.filtered_items in
  if len = 0 then
    { t with selected = 0 }
  else
    let selected = max 0 (min (len - 1) t.selected) in
    { t with selected }

let set_items = fun t ~items ->
  {
    t
    with all_items = items;
    filtered_items = items;
    selected = 0;
    scroll_offset = 0;
    filter_query = "";
  }

let string_contains = fun haystack needle ->
  (* Simple substring search *)
  let h_len = String.length haystack in
  let n_len = String.length needle in
  if n_len = 0 then
    true
  else if n_len > h_len then
    false
  else
    let rec search pos =
      if pos + n_len > h_len then
        false
      else
        let rec match_at i =
          if i >= n_len then
            true
          else if String.get haystack ~at:(pos + i) = Some (String.get_unchecked needle ~at:i) then
            match_at (i + 1)
          else
            false
        in
        if match_at 0 then
          true
        else
          search (pos + 1)
    in
    search 0

let apply_filter = fun t query ->
  if query = "" then
    { t with filtered_items = t.all_items; filter_query = "" }
  else
    let query_lower = String.lowercase_ascii query in
    let filtered =
      List.filter t.all_items
        ~fn:(fun item ->
          let rendered = String.lowercase_ascii (t.render item) in
          string_contains rendered query_lower)
    in
    { t with filtered_items = filtered; filter_query = query } |> clamp_selection

let set_filter = fun t ~filter:query -> apply_filter t query

let clear_filter = fun t ->
  { t with filter_query = ""; filtered_items = t.all_items; filtering_active = false }

let start_filtering = fun t ->
  if t.filter_enabled then
    { t with filtering_active = true }
  else
    t

let stop_filtering = fun t -> { t with filtering_active = false }

let select = fun t idx -> { t with selected = idx } |> clamp_selection

let select_next = fun t ->
  let len = List.length t.filtered_items in
  if len = 0 then
    t
  else
    { t with selected = min (len - 1) (t.selected + 1) }

let select_prev = fun t ->
  if List.length t.filtered_items = 0 then
    t
  else
    { t with selected = max 0 (t.selected - 1) }

let select_first = fun t -> { t with selected = 0 }

let select_last = fun t ->
  let len = List.length t.filtered_items in
  if len = 0 then
    t
  else
    { t with selected = len - 1 }

let handle_key = fun t (key: Event.key) modifier ->
  if t.filtering_active then
    match ((key: Event.key)) with
    | Event.Escape ->
        stop_filtering t
    | Event.Enter ->
        stop_filtering t
    | Event.Backspace when modifier = Event.NoModifier ->
        let query = t.filter_query in
        let len = String.length query in
        if len > 0 then
          let new_query = String.sub query 0 (len - 1) in
          apply_filter t new_query |> fun t -> { t with filtering_active = true }
        else
          t
    | Event.Key s when modifier = Event.NoModifier && String.length s = 1 ->
        let new_query = t.filter_query ^ s in
        apply_filter t new_query |> fun t -> { t with filtering_active = true }
    | _ ->
        t
  else
    (* Normal navigation mode *)
    match ((key: Event.key)) with
    | Event.Up
    | Event.Key "k" when modifier = Event.NoModifier -> select_prev t
    | Event.Down
    | Event.Key "j" when modifier = Event.NoModifier -> select_next t
    | Event.Key "g" when modifier = Event.NoModifier -> select_first t
    | Event.Key "G" when modifier = Event.Shift -> select_last t
    | Event.Home -> select_first t
    | Event.End -> select_last t
    | Event.Key "/" when modifier = Event.NoModifier && t.filter_enabled -> start_filtering t
    | Event.Escape when t.filter_query != "" -> clear_filter t
    | _ -> t

let view = fun t ->
  let open Buffer in
    let buf = create 256 in
    let items = t.filtered_items in
    let total = List.length items in
    if total = 0 then
      if t.filtering_active || t.filter_query != "" then
        add_string buf "No matches"
      else
        add_string buf "No items"
    else
      begin
        (* Determine visible window *)
        let start_idx, end_idx =
          if t.height = 0 then
            (0, total - 1)
          else
            (* Ensure selected item is visible *)
            let start = max 0 (min (total - t.height) t.selected) in
            (start, min (total - 1) (start + t.height - 1))
        in
        (* Render visible items *)
        List.iteri
          (fun idx item ->
            if idx >= start_idx && idx <= end_idx then
              begin
                let is_selected = idx = t.selected in
                let prefix =
                  if is_selected then
                    t.cursor_char
                  else
                    String.make (String.length t.cursor_char) ' '
                in
                let text = t.render item in
                let line =
                  if t.width > 0 && String.length text > t.width then
                    String.sub text 0 t.width
                  else
                    text
                in
                if idx > start_idx then
                  add_char buf '\n';
                add_string buf prefix;
                add_string buf line
              end)
          items
      end;
    (* Show filter input at bottom if active *)
    if t.filtering_active then
      begin
        add_string buf "\n\n/";
        add_string buf t.filter_query;
        add_char buf '_'
      end
    else if t.filter_query != "" then
      begin
        add_string buf "\n\nFilter: ";
        add_string buf t.filter_query;
        add_string buf " (press ESC to clear)"
      end;
    contents buf
