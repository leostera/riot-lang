(**
   * Example: Tab Interface
   *
   * This example demonstrates:
   * - Tab-based navigation
   * - Different content per tab
   * - Visual tab indicators
   * - Keyboard shortcuts for switching
   *
   * Key concepts:
   * - Conditional rendering based on state
   * - Styling active vs inactive tabs
   * - Layout composition with row and column
   *
   * Controls:
   * - Left/Right arrows or Tab - Switch tabs
   * - 1-4 - Jump to tab directly
   * - q/Escape - Quit
*)
open Std
open Minttea

(* Model: Current tab index *)

type model = {
  active_tab: int;
  tabs: (string * string) list;
}

(* Tab content *)

let tabs_data = [ (
    "Overview",
    "Welcome to the Tab Example!\n\n\
    This demonstrates how to create a tabbed interface\n\
    with different content for each tab.\n\n\
    • Use arrow keys or Tab to navigate\n\
    • Press number keys 1-4 to jump to a tab\n\
    • Each tab can have completely different content"
  ); (
    "Features",
    "Key Features:\n\n\
    ✓ Clean tab design\n\
    ✓ Keyboard navigation\n\
    ✓ Visual feedback for active tab\n\
    ✓ Flexible content area\n\
    ✓ Easy to extend with more tabs\n\n\
    You can add any content here including:\n\
    - Text\n\
    - Lists\n\
    - Forms\n\
    - Tables"
  ); (
    "Settings",
    "Settings Panel:\n\n\
    [ ] Enable notifications\n\
    [✓] Dark mode\n\
    [ ] Auto-save\n\
    [✓] Show line numbers\n\n\
    Theme: Dark\n\
    Font size: 14px\n\
    Tab width: 2 spaces"
  ); (
    "About",
    "About This Example\n\n\
    Version: 1.0.0\n\
    Author: Minttea Examples\n\
    License: MIT\n\n\
    Built with Minttea - A delightful TUI framework\n\
    for OCaml inspired by Bubble Tea.\n\n\
    Visit github.com/riot-ml/riot for more!"
  ); ]

(* Initialize *)

let init = fun model -> (model, Command.Noop)

(* Update: Handle tab switching *)

let update = fun event model ->
  let num_tabs = List.length model.tabs in
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Escape, _) ->
      (model, Command.Quit)
  | Event.KeyDown (Event.Left, _) ->
      let active_tab = max 0 (model.active_tab - 1) in
      ({ model with active_tab }, Command.Noop)
  | Event.KeyDown (Event.Right, _)
  | Event.KeyDown (Event.Tab, _) ->
      let active_tab = min (num_tabs - 1) (model.active_tab + 1) in
      ({ model with active_tab }, Command.Noop)
  | Event.KeyDown (Event.Key "1", _) when num_tabs > 0 ->
      ({ model with active_tab = 0 }, Command.Noop)
  | Event.KeyDown (Event.Key "2", _) when num_tabs > 1 ->
      ({ model with active_tab = 1 }, Command.Noop)
  | Event.KeyDown (Event.Key "3", _) when num_tabs > 2 ->
      ({ model with active_tab = 2 }, Command.Noop)
  | Event.KeyDown (Event.Key "4", _) when num_tabs > 3 ->
      ({ model with active_tab = 3 }, Command.Noop)
  | _ ->
      (model, Command.Noop)

(* Render a single tab *)

let render_tab = fun ~active index ((title, _)) ->
  let open Element in
    let is_active = active = index in
    let style =
      if is_active then
        Style.(empty
        |> bg (`rgb (62, 103, 224))
        |> fg (`rgb (255, 255, 255))
        |> bold
        |> padding (Style.Padding.symmetric ~h:2 ~v:1))
      else
        Style.(empty
        |> bg (`rgb (40, 40, 40))
        |> fg (`rgb (150, 150, 150))
        |> padding (Style.Padding.symmetric ~h:2 ~v:1))
    in
    text ~style (" " ^ Int.to_string (index + 1) ^ ":" ^ title ^ " ")

(* View: Render tabs and content *)

let view = fun model ->
  let open Element in
    let _, content = List.get_unchecked model.tabs ~at:model.active_tab in
    (* Render tab bar *)
    let tab_elements = model.tabs
    |> List.enumerate
    |> List.map ~fn:(fun (index, tab) -> render_tab ~active:model.active_tab index tab) in
    let tab_bar = row ~style:Style.(empty |> margin (Style.Margin.make ~bottom:1 ())) tab_elements in
    (* Content area *)
    let content_area = container
      ~style:Style.(empty
      |> border ~width:1 ~color:(`rgb (100, 100, 100)) ()
      |> padding (Style.Padding.all 2)
      |> min_height 15.0)
      [ text ~style:Style.(empty |> fg (`rgb (200, 200, 200))) content ] in
    (* Full layout *)
    column
      ~style:Style.(empty |> padding (Style.Padding.all 1))
      [
        tab_bar;
        content_area;
        text "";
        text ~style:Style.(empty |> fg (`rgb (100, 100, 100))) "← → Tab: Switch tabs • 1-4: Jump to tab • q: Quit";
      ]

(* Create and run the app *)

let app = App.make ~init ~update ~view ()

(* Run it *)

let main ~args:_ =
  let initial_model = { active_tab = 0; tabs = tabs_data } in
  let config = Minttea.config () in
  Minttea.run ~config initial_model app

let () = Runtime.run ~main ~args:Env.args ()
