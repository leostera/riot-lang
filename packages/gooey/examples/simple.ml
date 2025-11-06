open Std
open Gooey

let () =
  (* Create a simple UI *)
  let ui = Element.column ~style:Style.(empty |> padding (Padding.all 2)) [
    Element.text 
      ~style:Style.(empty |> bold |> fg (`rgb (59, 130, 246)))
      "Hello, Gooey!";
    
    Element.row ~style:Style.(empty |> child_gap 4) [
      Element.text ~style:Style.empty "Left";
      Element.spacer ~flex:1.0 ();
      Element.text ~style:Style.empty "Right";
    ];
    
    Element.text 
      ~style:Style.(empty |> fg (`rgb (150, 150, 150)))
      "This is a simple layout example";
  ] in
  
  (* Configure layout *)
  let config = Config.make
    ~viewport:(Viewport.make ~width:80.0 ~height:24.0)
    ~text_measurer:Config.default_text_measurer
    ()
  in
  
  (* Compute layout *)
  let commands = layout ~config ui in
  
  (* Print results *)
  print "Generated %d render commands:\n" (List.length commands);
  List.iter (fun cmd ->
    match cmd.Render.command_type with
    | Render.Rectangle { color; _ } ->
        let `rgb (r, g, b) = color in
        print "  Rectangle at (%.0f, %.0f) %.0fx%.0f color=(%d,%d,%d)\n"
          cmd.bounding_box.x cmd.bounding_box.y 
          cmd.bounding_box.width cmd.bounding_box.height
          r g b
    | Render.Text { content; _ } ->
        print "  Text at (%.0f, %.0f) %.0fx%.0f: '%s'\n"
          cmd.bounding_box.x cmd.bounding_box.y 
          cmd.bounding_box.width cmd.bounding_box.height
          content
    | Render.Border _ ->
        print "  Border at (%.0f, %.0f) %.0fx%.0f\n"
          cmd.bounding_box.x cmd.bounding_box.y 
          cmd.bounding_box.width cmd.bounding_box.height
    | Render.ScissorStart _ ->
        print "  ScissorStart\n"
    | Render.ScissorEnd ->
        print "  ScissorEnd\n"
    | Render.Custom _ ->
        print "  Custom\n"
  ) commands
