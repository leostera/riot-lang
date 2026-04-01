(**
 * Example: Viewport (Scrollable Content)
 * 
 * This example demonstrates:
 * - Using the Viewport component for scrollable content
 * - Vertical scrolling through long text
 * - Scroll position tracking
 * - Smooth scrolling
 * 
 * Key concepts:
 * - Creating viewports with fixed heights
 * - Handling scroll events
 * - Showing scroll indicators
 * 
 * Controls:
 * - Up/Down arrows - Scroll line by line
 * - Page Up/Down - Scroll by page
 * - Home/End - Jump to top/bottom
 * - q/Escape - Quit
 *)
open Std
open Minttea
open Minttea.Component

(* Model *)

type model = {
  viewport: Viewport.t;
  content: string;
}

(* Generate sample content *)

let generate_content = fun () ->
  let lines = ref [] in
  (* Add a header *)
  lines := "THE MINTTEA SCROLLING EXAMPLE" :: !lines;
  lines := "==============================" :: !lines;
  lines := "" :: !lines;
  (* Add some Lorem Ipsum *)
  lines := "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod" :: !lines;
  lines := "tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim" :: !lines;
  lines := "veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea" :: !lines;
  lines := "commodo consequat." :: !lines;
  lines := "" :: !lines;
  (* Add numbered lines *)
  lines := "Here are 100 numbered lines to demonstrate scrolling:" :: !lines;
  lines := "" :: !lines;
  for i = 1 to 100 do
    let line = Int.to_string i ^ ". This is line number " ^ Int.to_string i ^ " - scroll to see more content!" in
    lines := line :: !lines
  done;
  lines := "" :: !lines;
  lines := "THE END" :: !lines;
  lines := "" :: !lines;
  lines := "You've reached the bottom of the document!" :: !lines;
  String.concat "\n" (List.rev !lines)

(* Initialize *)

let init = fun model -> (model, Command.Noop)

(* Update *)

let update = fun event model ->
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Escape, _) ->
      (model, Command.Quit)
  | Event.KeyDown (Event.Up, _) ->
      let viewport = Viewport.scroll_up model.viewport ~lines:1 in
      ({model with viewport;}, Command.Noop)
  | Event.KeyDown (Event.Down, _) ->
      let viewport = Viewport.scroll_down model.viewport ~lines:1 in
      ({model with viewport;}, Command.Noop)
  | Event.KeyDown (Event.PageUp, _) ->
      let height = Viewport.height model.viewport in
      let viewport = Viewport.scroll_up model.viewport ~lines:height in
      ({model with viewport;}, Command.Noop)
  | Event.KeyDown (Event.PageDown, _) ->
      let height = Viewport.height model.viewport in
      let viewport = Viewport.scroll_down model.viewport ~lines:height in
      ({model with viewport;}, Command.Noop)
  | Event.KeyDown (Event.Home, _) ->
      (* Scroll to top by scrolling up by a large amount *)
      let viewport = Viewport.scroll_up model.viewport ~lines:10_000 in
      ({model with viewport;}, Command.Noop)
  | Event.KeyDown (Event.End, _) ->
      (* Scroll to bottom by scrolling down by a large amount *)
      let viewport = Viewport.scroll_down model.viewport ~lines:10_000 in
      ({model with viewport;}, Command.Noop)
  | _ ->
      (model, Command.Noop)

(* View *)

let view = fun model ->
  let open Element in
    let scroll_percent = Viewport.scroll_percent model.viewport in
    let at_top = Viewport.at_top model.viewport in
    let at_bottom = Viewport.at_bottom model.viewport in
    column ~style:Style.(empty |> padding (Padding.all 1))
      [
        text ~style:Style.(empty |> bold |> fg (`rgb (100, 200, 255))) "📜 Scrollable Content Viewer";
        text "";
        text ~style:Style.(empty |> fg (`rgb (150, 150, 150)))
          (
            "Scroll: " ^ Float.to_string (scroll_percent *. 100.0) ^ "% " ^ (
              if at_top then
                "(top)"
              else if at_bottom then
                "(bottom)"
              else
                ""
            )
          );
        container
          ~style:Style.(empty
          |> border ~width:1 ~color:(`rgb (100, 100, 200)) ()
          |> padding (Padding.all 1))
          [ text (Viewport.view model.viewport) ];
        text "";
        column
          ~style:Style.(empty |> fg (`rgb (100, 100, 100)))
          [
            text "Navigation:";
            text "  ↑↓        - Scroll line by line";
            text "  PgUp/PgDn - Scroll by page";
            text "  Home/End  - Jump to top/bottom";
            text "  q         - Quit";
          ];
      ]

(* Create and run the app *)

let app = App.make ~init ~update ~view ()

(* Run it *)

let () =
  let content = generate_content () in
  (* Create viewport with fixed height *)
  let viewport = Viewport.make ~width:70 ~height:15 |> Viewport.set_content ~content in
  let initial_model = {viewport;content;} in
  let config = Minttea.config () in
  Minttea.start ~config app initial_model
