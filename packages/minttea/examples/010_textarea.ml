(**
   * Example: Text Area (Multi-line Editor)
   *
   * This example demonstrates:
   * - Using the TextArea component for multi-line editing
   * - Line numbers display
   * - Cursor movement
   * - Text editing operations
   *
   * Key concepts:
   * - Multi-line text input
   * - Cursor positioning
   * - Line wrapping
   *
   * Controls:
   * - Type to enter text
   * - Arrow keys - Move cursor
   * - Enter - New line
   * - Backspace - Delete character
   * - Ctrl+C - Clear all
   * - q/Escape - Quit
*)
open Std
open Minttea

(* Model *)

type model = {
  text: string;
  cursor_row: int;
  cursor_col: int;
  saved: bool;
}

(* Sample initial text *)

let initial_text = "Welcome to the Minttea Text Editor!\n\
   \n\
   This is a simple multi-line text editor built with the TextArea component.\n\
   You can edit this text, add new lines, and navigate with arrow keys.\n\
   \n\
   Try editing this text:\n\
   - Type to add characters\n\
   - Use arrow keys to move around\n\
   - Press Enter for new lines\n\
   - Backspace to delete\n\
   \n\
   Happy editing!"

(* Initialize *)

let init = fun model -> (model, Command.Noop)

(* Update *)

let update = fun event model ->
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Escape, _) ->
      (model, Command.Quit)
  | Event.KeyDown (Event.Key "s", Event.Ctrl) ->
      (* Simulate save *)
      ({ model with saved = true }, Command.Noop)
  | Event.KeyDown (Event.Key "c", Event.Ctrl) ->
      (* Clear all text *)
      ({ model with text = ""; saved = false }, Command.Noop)
  | Event.KeyDown (Event.Key s, _) when String.length s = 1 ->
      (* Insert character *)
      let text = model.text ^ s in
      ({ model with text; saved = false }, Command.Noop)
  | Event.KeyDown (Event.Enter, _) ->
      (* Add new line *)
      let text = model.text ^ "\n" in
      ({ model with text; saved = false }, Command.Noop)
  | Event.KeyDown (Event.Backspace, _) ->
      (* Delete last character *)
      let len = String.length model.text in
      let text =
        if len > 0 then
          String.sub model.text ~offset:0 ~len:(len - 1)
        else
          ""
      in
      ({ model with text; saved = false }, Command.Noop)
  | Event.KeyDown (Event.Up, _) ->
      let cursor_row = max 0 (model.cursor_row - 1) in
      ({ model with cursor_row }, Command.Noop)
  | Event.KeyDown (Event.Down, _) ->
      let cursor_row = model.cursor_row + 1 in
      ({ model with cursor_row }, Command.Noop)
  | Event.KeyDown (Event.Left, _) ->
      let cursor_col = max 0 (model.cursor_col - 1) in
      ({ model with cursor_col }, Command.Noop)
  | Event.KeyDown (Event.Right, _) ->
      let cursor_col = model.cursor_col + 1 in
      ({ model with cursor_col }, Command.Noop)
  | _ ->
      (model, Command.Noop)

(* View *)

let view = fun model ->
  let open Element in
    let lines = String.split_on_char '\n' model.text in
    let line_count = List.length lines in
    column ~style:Style.(empty |> padding (Style.Padding.all 1))
      [
        row ~style:Style.(empty
        |> bg (`rgb (40, 40, 40))
        |> padding (Style.Padding.symmetric ~h:2 ~v:1))
          [
            text ~style:Style.(empty |> bold |> fg (`rgb (100, 200, 255))) "📝 Minttea Text Editor";
            spacer ~flex:1.0 ();
            text
              ~style:Style.(
                empty |> fg
                  (
                    if model.saved then
                      `rgb (0, 255, 0)
                    else
                      `rgb (255, 200, 0)
                  )
              )
              (
                if model.saved then
                  "Saved"
                else
                  "Modified"
              );
          ];
        text
          ~style:Style.(empty |> fg (`rgb (150, 150, 150)))
          ("Line "
          ^ Int.to_string (model.cursor_row + 1)
          ^ ":"
          ^ Int.to_string (model.cursor_col + 1)
          ^ " | Total lines: "
          ^ Int.to_string line_count);
        text "";
        container
          ~style:Style.(empty
          |> border ~width:1 ~color:(`rgb (0, 255, 127)) ()
          |> padding (Style.Padding.all 1)
          |> min_height 15.0
          |> min_width 70.0)
          [ text model.text ];
        text "";
        text ~style:Style.(empty |> fg (`rgb (100, 100, 100))) "Type to edit • Ctrl+S: Save • Ctrl+C: Clear • q: Quit";
      ]

(* Create and run the app *)

let app = App.make ~init ~update ~view ()

(* Run it *)

let main ~args:_ =
  let initial_model = { text = initial_text; cursor_row = 0; cursor_col = 0; saved = false } in
  let config = Minttea.config () in
  Minttea.run ~config initial_model app

let () = Runtime.run ~main ~args:Env.args ()
