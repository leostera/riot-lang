open Std

(**
   Terminal size detection.

   Provides functions to query the terminal's dimensions (rows and columns).
   Useful for responsive terminal UIs and layout calculations.

   ## Examples

   Get terminal size:
   ```ocaml
   match Size.get () with
   | Ok { rows; cols } ->
       Printf.printf "Terminal is %d columns × %d rows\n" cols rows
   | Error (`System_error msg) ->
       Printf.eprintf "Error: %s\n" msg
   ```

   Center text in terminal:
   ```ocaml
   let center_text text =
     match Size.get () with
     | Ok { cols; _ } ->
         let text_len = String.length text in
         let padding = (cols - text_len) / 2 in
         String.make padding ' ' ^ text
     | Error _ -> text

   let () =
     Terminal.clear () |> print_string;
     print_endline (center_text "Welcome!");
     flush stdout
   ```

   Adaptive layout:
   ```ocaml
   let draw_ui () =
     match Size.get () with
     | Ok { rows; cols } when cols >= 80 && rows >= 24 ->
         draw_full_ui ()
     | Ok { cols; _ } when cols >= 40 ->
         draw_compact_ui ()
     | Ok _ ->
         draw_minimal_ui ()
     | Error _ ->
         draw_fallback_ui ()
   ```
*)
type t = {
  rows: int;
  (** Number of rows (lines) in terminal *)
  cols: int;
  (** Number of columns (characters per line) in terminal *)
}

val get: unit -> (t, [`System_error of string]) result

(**
   Get current terminal size.

   Queries the terminal for its dimensions using system calls.

   Returns:
   - [Ok { rows; cols }] on success
   - [Error (`System_error msg)] if unable to determine size

   Examples:
   ```ocaml
   (* Simple usage *)
   let size = Size.get () |> Result.get_ok in
   Printf.printf "%dx%d\n" size.cols size.rows

   (* With error handling *)
   match Size.get () with
   | Ok size ->
       if size.cols < 80 then
         print_endline "Terminal too narrow!"
   | Error (`System_error msg) ->
       Printf.eprintf "Failed: %s\n" msg

   (* With default fallback *)
   let size =
     Size.get ()
     |> Result.value ~default:{ rows = 24; cols = 80 }
   ```

   Note: Size may change if user resizes terminal window.
*)
val to_string: t -> string

(**
   Convert size to human-readable string.

   Examples:
   ```ocaml
   let size = { Size.rows = 24; cols = 80 } in
   Size.to_string size
   (* "{ rows = 24; cols = 80 }" *)
   ```
*)
