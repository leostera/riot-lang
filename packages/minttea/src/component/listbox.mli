(**
   Navigable list component with filtering.

   Displays a vertical list of items that can be navigated with keyboard controls.
   Supports selection, filtering, and custom item rendering.

   ## Example: Simple List

   ```ocaml
   open Std
   open Minttea

   type model = { list : string Listbox.t }

   let items = ["Apple"; "Banana"; "Cherry"; "Date"; "Elderberry"]

   let init () =
     let list = Listbox.make items
       |> Listbox.set_height 10
     in
     ({ list }, Command.Noop)

   let update event model =
     match event with
     | Event.KeyDown (key, mods) ->
         let list = Listbox.handle_key model.list key mods in
         ({ list }, Command.Noop)
     | _ -> (model, Command.Noop)

   let view model =
     Listbox.view model.list
   ```

   ## Example: Custom Item Rendering

   ```ocaml
   type item = { name : string; count : int }

   let render_item ~selected item =
     let prefix = if selected then "> " else "  " in
     format "%s%s (%d)" prefix item.name item.count

   let items = [
     { name = "Tasks"; count = 5 };
     { name = "Notes"; count = 12 };
   ]

   let list = Listbox.make ~render:render_item items
   ```
*)
open Std

type 'a t

(** A list instance containing items of type `'a` *)
val make: ?render:('a -> string) -> 'a list -> 'a t

(**
   `make ?render items` creates a new list from items.

   - `render` - Optional custom rendering function. Default uses `to_string`.
   - `items` - List of items to display
*)
val set_height: 'a t -> height:int -> 'a t

(**
   `set_height list h` sets the visible height (number of items shown).

   Set to 0 for unlimited height (show all items).
*)
val set_width: 'a t -> width:int -> 'a t

(**
   `set_width list w` sets the display width (for line wrapping/truncation).

   Set to 0 for unlimited width.
*)
val set_cursor_char: 'a t -> char:string -> 'a t

(**
   `set_cursor_char list char` sets the character shown before selected item.

   Default: "> "
*)
val set_filter_enabled: 'a t -> enabled:bool -> 'a t

(**
   `set_filter_enabled list enabled` enables or disables filtering.

   When enabled, pressing '/' enters filter mode.
*)
val items: 'a t -> 'a list

(** `items list` returns all items (unfiltered). *)
val visible_items: 'a t -> 'a list

(** `visible_items list` returns currently visible items (after filtering). *)
val set_items: 'a t -> items:'a list -> 'a t

(** `set_items list items` replaces all items. Resets selection to first item. *)
val selected_item: 'a t -> 'a option

(** `selected_item list` returns the currently selected item, if any. *)
val selected_index: 'a t -> int option

(** `selected_index list` returns the index of the selected item in visible items. *)
val select: 'a t -> int -> 'a t

(**
   `select list idx` selects item at index (in visible items).

   Index is clamped to valid range.
*)
val select_next: 'a t -> 'a t

(** `select_next list` moves selection down one item. *)
val select_prev: 'a t -> 'a t

(** `select_prev list` moves selection up one item. *)
val select_first: 'a t -> 'a t

(** `select_first list` selects the first visible item. *)
val select_last: 'a t -> 'a t

(** `select_last list` selects the last visible item. *)
val filter_query: 'a t -> string

(** `filter_query list` returns the current filter query string. *)
val set_filter: 'a t -> filter:string -> 'a t

(**
   `set_filter list query` applies a filter query.

   Filters items by substring match on rendered text.
   Empty string clears filter.
*)
val clear_filter: 'a t -> 'a t

(** `clear_filter list` clears the current filter. *)
val is_filtering: 'a t -> bool

(** `is_filtering list` returns true if user is in filter input mode. *)
val start_filtering: 'a t -> 'a t

(** `start_filtering list` enters filter input mode. *)
val stop_filtering: 'a t -> 'a t

(** `stop_filtering list` exits filter input mode, keeps current filter. *)
val handle_key: 'a t -> Event.key -> Event.modifier -> 'a t

(**
   `handle_key list key modifier` processes keyboard input.

   Default bindings:
   - Up/k: select previous
   - Down/j: select next
   - g: select first
   - G: select last
   - /: start filtering (if enabled)
   - Escape: stop filtering / clear filter
   - Character keys (in filter mode): update filter

   Returns updated list.
*)
val view: 'a t -> string

(**
   `view list` renders the list for display.

   Format:
   ```
     Item 1
   > Item 2  (selected)
     Item 3
   ```

   If filtering is active, shows filter input at bottom.
   If height is set, shows only visible window of items.
*)
