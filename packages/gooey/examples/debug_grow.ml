open Std
open Gooey

(* Debug program to print layout widths *)

let text_measurer = fun text _style ->
  let width = float_of_int (String.length text) in
  let height = 1.0 in
  Viewport.make ~width ~height

let () =
  (* Create UI: Three columns (Fixed 20, Grow, Fixed 15) *)
  let ui = Element.row
  [
    Element.container
    ~style:((Style.empty |> Style.width (Style.Fixed 20.0)))
    [ Element.text "Left" ];
    Element.container ~style:((Style.empty |> Style.width Style.Grow)) [ Element.text "Middle" ];
    Element.container
    ~style:((Style.empty |> Style.width (Style.Fixed 15.0)))
    [ Element.text "Right" ];

  ] in
  (* Layout with 80x24 viewport *)
  let viewport = Viewport.make ~width:80.0 ~height:24.0 in
  let config = Config.make ~viewport ~text_measurer () in
  let commands = Gooey.layout ~config ui in
  (* Print all rectangle widths *)
  println ("Total commands: " ^ Int.to_string (List.length commands));
  List.iter
    (fun cmd ->
      match cmd.Render.command_type with
      | Render.Rectangle _ -> println
      ("Rectangle: x="
      ^ Float.to_string cmd.bounding_box.x
      ^ " y="
      ^ Float.to_string cmd.bounding_box.y
      ^ " w="
      ^ Float.to_string cmd.bounding_box.width
      ^ " h="
      ^ Float.to_string cmd.bounding_box.height)
      | Render.Text txt -> println
      ("Text '"
      ^ txt.content
      ^ "': x="
      ^ Float.to_string cmd.bounding_box.x
      ^ " y="
      ^ Float.to_string cmd.bounding_box.y
      ^ " w="
      ^ Float.to_string cmd.bounding_box.width
      ^ " h="
      ^ Float.to_string cmd.bounding_box.height)
      | _ -> ())
    commands
