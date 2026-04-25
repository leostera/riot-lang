open Std
open Gooey

let main ~args:_ =
  (* Create a simple UI *)
  let ui = Element.column
    ~style:Style.(empty |> padding (Style.Padding.all 2))
    [
      Element.text ~style:Style.(empty |> bold |> fg (`rgb (59, 130, 246))) "Hello, Gooey!";
      Element.row
        ~style:Style.(empty |> child_gap 4)
        [
          Element.text ~style:Style.empty "Left";
          Element.spacer ~flex:1.0 ();
          Element.text ~style:Style.empty "Right"
        ];
      Element.text ~style:Style.(empty |> fg (`rgb (150, 150, 150))) "This is a simple layout example"
    ] in
  (* Configure layout *)
  let config = Config.make
    ~viewport:(Viewport.make ~width:80.0 ~height:24.0)
    ~text_measurer:Config.default_text_measurer
    () in
  (* Compute layout *)
  let commands = layout ~config ui in
  (* Print results *)
  println ("Generated " ^ Int.to_string (List.length commands) ^ " render commands:");
  List.for_each commands
    ~fn:(fun cmd ->
      match cmd.Render.command_type with
      | Render.Rectangle { color; _ } ->
          let `rgb (r, g, b) = color in
          println
            ("  Rectangle at ("
            ^ Float.to_string cmd.bounding_box.x
            ^ ", "
            ^ Float.to_string cmd.bounding_box.y
            ^ ") "
            ^ Float.to_string cmd.bounding_box.width
            ^ "x"
            ^ Float.to_string cmd.bounding_box.height
            ^ " color=("
            ^ Int.to_string r
            ^ ","
            ^ Int.to_string g
            ^ ","
            ^ Int.to_string b
            ^ ")")
      | Render.Text { content; _ } ->
          println
            ("  Text at ("
            ^ Float.to_string cmd.bounding_box.x
            ^ ", "
            ^ Float.to_string cmd.bounding_box.y
            ^ ") "
            ^ Float.to_string cmd.bounding_box.width
            ^ "x"
            ^ Float.to_string cmd.bounding_box.height
            ^ ": '"
            ^ content
            ^ "'")
      | Render.Border _ ->
          println
            ("  Border at ("
            ^ Float.to_string cmd.bounding_box.x
            ^ ", "
            ^ Float.to_string cmd.bounding_box.y
            ^ ") "
            ^ Float.to_string cmd.bounding_box.width
            ^ "x"
            ^ Float.to_string cmd.bounding_box.height)
      | Render.ScissorStart _ ->
          println "  ScissorStart"
      | Render.ScissorEnd ->
          println "  ScissorEnd"
      | Render.Custom _ ->
          println "  Custom");
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
