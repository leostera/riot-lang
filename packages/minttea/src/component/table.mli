(**
   Tabular data display component.

   Displays structured data with columns and rows.
   Supports navigation, selection, scrolling, and customizable styling.

   ## Example: Basic Table

   ```ocaml
   open Std
   open Minttea

   type model = { table : Table.t }

   let init () =
     let columns = [
       Table.column ~title:"ID" ~width:5;
       Table.column ~title:"Name" ~width:20;
       Table.column ~title:"Status" ~width:10;
     ] in
     let rows = [
       ["1"; "Alice"; "Active"];
       ["2"; "Bob"; "Inactive"];
       ["3"; "Charlie"; "Active"];
     ] in
     let table = Table.make columns rows
       |> Table.set_height 10
       |> Table.focus
     in
     ({ table }, Command.Noop)

   let update event model =
     match event with
     | Event.KeyDown (key, mods) ->
         let table = Table.handle_key model.table key mods in
         ({ table }, Command.Noop)
     | _ -> (model, Command.Noop)

   let view model =
     Table.view model.table
   ```

   ## Example: With Row Selection

   ```ocaml
   let view model =
     let table_view = Table.view model.table in
     match Table.selected_row model.table with
     | Some row ->
         table_view ^ "\n\nSelected: " ^ String.concat ", " row
     | None ->
         table_view
   ```
*)
open Std

(** A table instance. *)
type t
(** A column definition with title and width. *)
type column
(** A row is a list of cell values, one per column. *)
type row = string list

val column: title:string -> width:int -> column

(**
   `column ~title ~width` creates a column definition.

   - `title` - The column header text
   - `width` - The column width in characters
*)
val make: column list -> row list -> t

(**
   `make columns rows` creates a new table.

   - `columns` - List of column definitions (headers)
   - `rows` - List of data rows

   Note: Each row should have the same number of cells as columns.
*)
val set_height: t -> height:int -> t

(**
   `set_height table h` sets the visible height (number of rows shown).

   Set to 0 for unlimited height. Header is not counted in height.
*)
val set_width: t -> width:int -> t

(**
   `set_width table w` sets the total table width.

   Columns will be sized according to their individual widths.
*)
val set_show_header: t -> show:bool -> t

(** `set_show_header table show` controls header visibility. Default: true *)
val set_cursor_char: t -> char:string -> t

(** `set_cursor_char table char` sets the selection indicator. Default: "> " *)
val columns: t -> column list

(** `columns table` returns the column definitions. *)
val rows: t -> row list

(** `rows table` returns all rows. *)
val set_columns: t -> columns:column list -> t

(** `set_columns table cols` updates column definitions. *)
val set_rows: t -> rows:row list -> t

(** `set_rows table rows` replaces all rows. Resets selection to first row. *)
val selected_row: t -> row option

(** `selected_row table` returns the currently selected row, if any. *)
val selected_index: t -> int option

(** `selected_index table` returns the index of the selected row (0-based). *)
val cursor: t -> int

(** `cursor table` returns the cursor position (same as selected_index but as int). *)
val select: t -> int -> t

(** `select table idx` selects row at index (clamped to valid range). *)
val move_up: t -> int -> t

(** `move_up table n` moves selection up by n rows. *)
val move_down: t -> int -> t

(** `move_down table n` moves selection down by n rows. *)
val goto_top: t -> t

(** `goto_top table` selects the first row. *)
val goto_bottom: t -> t

(** `goto_bottom table` selects the last row. *)
val focus: t -> t

(** `focus table` enables keyboard navigation and shows selection. *)
val blur: t -> t

(** `blur table` disables keyboard navigation and hides selection. *)
val is_focused: t -> bool

(** `is_focused table` returns true if table has focus. *)
val handle_key: t -> Event.key -> Event.modifier -> t

(**
   `handle_key table key modifier` processes keyboard input.

   Default bindings:
   - Up/k: move up one row
   - Down/j: move down one row
   - Page Up/b: move up one page
   - Page Down/f/Space: move down one page
   - Ctrl+U/u: move up half page
   - Ctrl+D/d: move down half page
   - Home/g: goto first row
   - End/G: goto last row

   Returns updated table. No-op if not focused.
*)
val view: t -> string

(**
   `view table` renders the table for display.

     Format:
     ```
     ID   Name                Status
     ───  ──────────────────  ──────────
     1    Alice               Active
   > 2    Bob                 Inactive
     3    Charlie             Active
     ```

     Selection is indicated by cursor char.
     If height is set, shows scrolling window of rows.
*)
