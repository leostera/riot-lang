(**
   * Example: Simple List Selection
   *
   * This example demonstrates:
   * - Using the Listbox component
   * - Arrow key navigation
   * - Item selection with Enter
   * - Returning selected value
   *
   * Key concepts:
   * - Component state management
   * - Handling keyboard input for components
   * - Getting selected values from components
   *
   * Controls:
   * - Up/Down arrows - Navigate list
   * - Enter - Select item and quit
   * - q/Escape - Quit without selecting
*)
open Std
open Minttea
open Minttea.Component

(* Model: Just the listbox component *)

type model = {
  list: string Listbox.t;
  selected: string option;
}

(* Sample items for the list *)

let items = [
  "🍎 Apple";
  "🍌 Banana";
  "🍒 Cherry";
  "🍓 Strawberry";
  "🍊 Orange";
  "🍇 Grapes";
  "🥝 Kiwi";
  "🍑 Peach";
  "🍉 Watermelon";
  "🫐 Blueberry";
]

(* Initialize: Create the listbox *)

let init = fun model -> (model, Command.Noop)

(* Update: Handle events *)

let update = fun event model ->
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Escape, _) ->
      (model, Command.Quit)
  | Event.KeyDown (Event.Enter, _) ->
      (* Get the selected item and quit *)
      let selected = Listbox.selected_item model.list in
      ({ model with selected }, Command.Quit)
  | Event.KeyDown (Event.Up, _) ->
      let list = Listbox.select_prev model.list in
      ({ model with list }, Command.Noop)
  | Event.KeyDown (Event.Down, _) ->
      let list = Listbox.select_next model.list in
      ({ model with list }, Command.Noop)
  | _ ->
      (model, Command.Noop)

(* View: Render the listbox *)

let view = fun model ->
  let open Element in
    column ~style:Style.(empty |> padding (Style.Padding.all 1))
      [
        text (Listbox.view model.list);
        text "";
        text ~style:Style.(empty |> fg (`rgb (100, 100, 100))) "↑↓ Navigate • Enter Select • q Quit";
        (
          match model.selected with
          | Some item -> text
            ~style:Style.(empty |> fg (`rgb (0, 255, 0)) |> padding (Style.Padding.make ~top:1 ()))
            ("You selected: " ^ item)
          | None -> empty
        );
      ]

(* Create and run the app *)

let app = App.make ~init ~update ~view ()

(* Run it *)

let main ~args:_ =
  let initial_model = { list = Listbox.make items |> Listbox.set_height ~height:10; selected = None } in
  let config = Minttea.config () in
  Minttea.run ~config initial_model app

let () = Runtime.run ~main ~args:Env.args ()
