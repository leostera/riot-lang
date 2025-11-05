open Std

let test_paint_simple_text () =
  let module M = Minttea.Render.Matrix in
  let module S = Minttea.Render.Scene in
  let module P = Minttea.Render.Painter in
  
  let matrix = M.create ~width:5 ~height:2 in
  
  (* Create a simple text node *)
  let rect = S.{x = 0; y = 0; width = 5; height = 1} in
  let style = S.{
    fg = None;
    bg = None;
    bold = false;
    italic = false;
    underline = false;
    strikethrough = false;
    reverse = false;
  } in
  let node = S.text_node ~rect ~z_index:0 ~style "Hello" in
  
  (* Paint it *)
  P.paint ~matrix ~scene:[node];
  
  (* Check the painted text *)
  match M.get matrix ~x:0 ~y:0 with
  | Some cell when cell.M.char = "H" -> Ok ()
  | _ -> Error "Expected 'H' at (0,0)"

let test_paint_overlapping_z_index () =
  let module M = Minttea.Render.Matrix in
  let module S = Minttea.Render.Scene in
  let module P = Minttea.Render.Painter in
  
  let matrix = M.create ~width:5 ~height:2 in
  
  let style = S.{
    fg = None; bg = None; bold = false; italic = false;
    underline = false; strikethrough = false; reverse = false;
  } in
  
  (* Bottom layer (z=0) *)
  let rect1 = S.{x = 0; y = 0; width = 3; height = 1} in
  let node1 = S.text_node ~rect:rect1 ~z_index:0 ~style "AAA" in
  
  (* Top layer (z=1) - overlaps at position (1, 0) *)
  let rect2 = S.{x = 1; y = 0; width = 2; height = 1} in
  let node2 = S.text_node ~rect:rect2 ~z_index:1 ~style "BB" in
  
  (* Sort by z-index before painting *)
  let scene = S.sort_by_z [node1; node2] in
  
  (* Paint *)
  P.paint ~matrix ~scene;
  
  (* Check: should be "ABB" (node2 painted over node1) *)
  let check_char x expected =
    match M.get matrix ~x ~y:0 with
    | Some cell when cell.M.char = expected -> true
    | _ -> false
  in
  
  if check_char 0 "A" && check_char 1 "B" && check_char 2 "B" then Ok ()
  else Error "Z-index ordering not respected"

let test_paint_with_clipping () =
  let module M = Minttea.Render.Matrix in
  let module S = Minttea.Render.Scene in
  let module P = Minttea.Render.Painter in
  
  let matrix = M.create ~width:5 ~height:2 in
  
  let style = S.{
    fg = None; bg = None; bold = false; italic = false;
    underline = false; strikethrough = false; reverse = false;
  } in
  
  (* Create a text node at (0,0) with size 5x1, but clip to (0,0)-(2,1) *)
  let rect = S.{x = 0; y = 0; width = 5; height = 1} in
  let clip = S.{x = 0; y = 0; width = 2; height = 1} in
  
  let node = S.{
    rect;
    z_index = 0;
    content = TextNode {text = "Hello"; style};
    clip = Some clip;
  } in
  
  (* Paint *)
  P.paint ~matrix ~scene:[node];
  
  (* Check: only "He" should be painted *)
  let check_pos x expected_char =
    match M.get matrix ~x ~y:0 with
    | Some cell -> cell.M.char = expected_char
    | None -> false
  in
  
  (* First two chars painted *)
  let ok1 = check_pos 0 "H" in
  let ok2 = check_pos 1 "e" in
  
  (* Third char should still be space (not painted) *)
  let ok3 = check_pos 2 " " in
  
  if ok1 && ok2 && ok3 then Ok ()
  else Error "Clipping not working correctly"

let test_text_preserves_existing_background () =
  let module M = Minttea.Render.Matrix in
  let module S = Minttea.Render.Scene in
  let module P = Minttea.Render.Painter in
  
  let matrix = M.create ~width:10 ~height:3 in
  let blue = Tty.Color.of_rgb (0, 0, 255) in
  let white = Tty.Color.of_rgb (255, 255, 255) in
  
  (* First, paint a container with blue background *)
  let container_rect = S.{x = 0; y = 0; width = 10; height = 3} in
  let container_style = S.{
    fg = None;
    bg = Some blue;
    bold = false;
    italic = false;
    underline = false;
    strikethrough = false;
    reverse = false;
  } in
  let container = S.container ~rect:container_rect ~z_index:0 ~style:container_style [] in
  
  (* Then paint text with white foreground but NO background *)
  let text_rect = S.{x = 2; y = 1; width = 5; height = 1} in
  let text_style = S.{
    fg = Some white;
    bg = None;  (* No background specified! *)
    bold = false;
    italic = false;
    underline = false;
    strikethrough = false;
    reverse = false;
  } in
  let text = S.text_node ~rect:text_rect ~z_index:1 ~style:text_style "Hello" in
  
  (* Paint both (container first, then text) *)
  let scene = S.sort_by_z [container; text] in
  P.paint ~matrix ~scene;
  
  (* Check that text cells have blue background preserved *)
  let check_cell x y expected_char expected_bg =
    match M.get matrix ~x ~y with
    | Some cell -> 
        cell.M.char = expected_char && cell.M.bg = expected_bg
    | None -> false
  in
  
  (* First char of "Hello" at (2, 1) should have white fg AND blue bg *)
  let ok1 = check_cell 2 1 "H" (Some blue) in
  let ok2 = check_cell 3 1 "e" (Some blue) in
  
  (* Background-only cells should also have blue *)
  let ok3 = check_cell 0 0 " " (Some blue) in
  
  if ok1 && ok2 && ok3 then Ok ()
  else Error "Text did not preserve existing background from container"

let tests =
  Test.[
    case "paint simple text" test_paint_simple_text;
    case "paint overlapping z-index" test_paint_overlapping_z_index;
    case "paint with clipping" test_paint_with_clipping;
    case "text preserves existing background" test_text_preserves_existing_background;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"painter" ~tests ~args)
    ~args:Env.args ()
