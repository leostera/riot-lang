open Std

(** Tests for Element -> Scene graph conversion (Layout phase) *)

module E = Minttea.Element
module S = Minttea.Style
module Scene = Minttea.Render.Scene
module Layout = Minttea.Render.Layout

(** Helper: Create a layout context *)
let make_ctx ~x ~y ~width ~height =
  Layout.{ x; y; available_width = width; available_height = height }

(** Helper: Extract rect from scene node *)
let get_rect node = node.Scene.rect

(** Helper: Extract style from scene node *)
let get_style node = 
  match node.Scene.content with
  | Scene.TextNode { style; _ } -> Some style
  | Scene.Container { style; _ } -> style

(** Test 1: Simple box creates container with correct rect *)
let test_box_creates_container () =
  let elem = E.box E.empty in
  let ctx = make_ctx ~x:0 ~y:0 ~width:10 ~height:5 in
  let scene = Layout.to_scene elem ctx in
  
  let rect = get_rect scene in
  if rect.x = 0 && rect.y = 0 && rect.width = 10 && rect.height = 5 then
    Ok ()
  else
    Error (format "Expected rect (0,0,10x5), got (%d,%d,%dx%d)" 
      rect.x rect.y rect.width rect.height)

(** Test 2: Box with fixed size uses fixed dimensions *)
let test_box_fixed_size () =
  let elem = E.box 
    ~style:(S.default |> S.width_fixed 20 |> S.height_fixed 8)
    (E.empty) 
  in
  let ctx = make_ctx ~x:0 ~y:0 ~width:100 ~height:50 in
  let scene = Layout.to_scene elem ctx in
  
  let rect = get_rect scene in
  (* Box should still use available space from parent, not fixed size *)
  (* Fixed size applies to children in Row/Column layouts *)
  if rect.width = 100 && rect.height = 50 then
    Ok ()
  else
    Error (format "Expected rect with size 100x50, got %dx%d" rect.width rect.height)

(** Test 3: Box with padding reduces child's available space *)
let test_box_with_padding () =
  let elem = E.box 
    ~style:(S.default 
      |> S.padding_left 2 
      |> S.padding_right 3
      |> S.padding_top 1
      |> S.padding_bottom 1)
    (E.text "hello") 
  in
  let ctx = make_ctx ~x:0 ~y:0 ~width:10 ~height:5 in
  let scene = Layout.to_scene elem ctx in
  
  (* Box rect should be full size *)
  let rect = get_rect scene in
  if rect.width <> 10 || rect.height <> 5 then
    Error (format "Box rect should be 10x5, got %dx%d" rect.width rect.height)
  else
    (* Check if it's a container with children *)
    match scene.content with
    | Scene.Container { children; _ } ->
        if List.length children = 0 then
          Error "Container should have child"
        else
          let child = List.hd children in
          let child_rect = get_rect child in
          (* Child should be positioned at (2,1) with size (5,3) *)
          (* width: 10 - 2 (left) - 3 (right) = 5 *)
          (* height: 5 - 1 (top) - 1 (bottom) = 3 *)
          if child_rect.x = 2 && child_rect.y = 1 && 
             child_rect.width = 5 && child_rect.height = 3 then
            Ok ()
          else
            Error (format "Child rect should be (2,1,5x3), got (%d,%d,%dx%d)"
              child_rect.x child_rect.y child_rect.width child_rect.height)
    | _ -> Error "Expected Container node"

(** Test 4: Text node creates correct scene *)
let test_text_creates_text_node () =
  let elem = E.text "Hello" in
  let ctx = make_ctx ~x:5 ~y:10 ~width:20 ~height:3 in
  let scene = Layout.to_scene elem ctx in
  
  let rect = get_rect scene in
  if rect.x <> 5 || rect.y <> 10 || rect.width <> 20 || rect.height <> 3 then
    Error (format "Text rect should be (5,10,20x3), got (%d,%d,%dx%d)"
      rect.x rect.y rect.width rect.height)
  else
    match scene.content with
    | Scene.TextNode { text; _ } ->
        if text = "Hello" then Ok ()
        else Error (format "Expected text 'Hello', got '%s'" text)
    | _ -> Error "Expected TextNode"

(** Test 5: Row with fixed-width children *)
let test_row_fixed_widths () =
  let elem = E.row [
    E.box ~style:(S.default |> S.width_fixed 10) (E.text "A");
    E.box ~style:(S.default |> S.width_fixed 20) (E.text "B");
    E.box ~style:(S.default |> S.width_fixed 15) (E.text "C");
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:100 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      if List.length children <> 3 then
        Error (format "Expected 3 children, got %d" (List.length children))
      else
        let rects = List.map get_rect children in
        let rect1 = List.nth rects 0 in
        let rect2 = List.nth rects 1 in
        let rect3 = List.nth rects 2 in
        
        (* Check widths *)
        if rect1.width <> 10 || rect2.width <> 20 || rect3.width <> 15 then
          Error (format "Expected widths [10,20,15], got [%d,%d,%d]"
            rect1.width rect2.width rect3.width)
        (* Check x positions *)
        else if rect1.x <> 0 || rect2.x <> 10 || rect3.x <> 30 then
          Error (format "Expected x positions [0,10,30], got [%d,%d,%d]"
            rect1.x rect2.x rect3.x)
        else
          Ok ()
  | _ -> Error "Expected Container node"

(** Test 6: Row with flex-width children *)
let test_row_flex_widths () =
  let elem = E.row [
    E.box ~style:(S.default |> S.width_flex 1.0) (E.text "A");
    E.box ~style:(S.default |> S.width_flex 2.0) (E.text "B");
    E.box ~style:(S.default |> S.width_flex 1.0) (E.text "C");
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:40 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      let rects = List.map get_rect children in
      let widths = List.map (fun r -> r.Scene.width) rects in
      (* Total flex = 4.0, available = 40 *)
      (* Child A: 40 * (1/4) = 10 *)
      (* Child B: 40 * (2/4) = 20 *)
      (* Child C: 40 * (1/4) = 10 *)
      if widths = [10; 20; 10] then
        Ok ()
      else
        Error (format "Expected widths [10,20,10], got [%d;%d;%d]"
          (List.nth widths 0) (List.nth widths 1) (List.nth widths 2))
  | _ -> Error "Expected Container node"

(** Test 7: Column with fixed-height children *)
let test_column_fixed_heights () =
  let elem = E.column [
    E.box ~style:(S.default |> S.height_fixed 5) (E.text "A");
    E.box ~style:(S.default |> S.height_fixed 10) (E.text "B");
    E.box ~style:(S.default |> S.height_fixed 8) (E.text "C");
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:20 ~height:100 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      if List.length children <> 3 then
        Error (format "Expected 3 children, got %d" (List.length children))
      else
        let rects = List.map get_rect children in
        let rect1 = List.nth rects 0 in
        let rect2 = List.nth rects 1 in
        let rect3 = List.nth rects 2 in
        
        (* Check heights *)
        if rect1.height <> 5 || rect2.height <> 10 || rect3.height <> 8 then
          Error (format "Expected heights [5,10,8], got [%d,%d,%d]"
            rect1.height rect2.height rect3.height)
        (* Check y positions *)
        else if rect1.y <> 0 || rect2.y <> 5 || rect3.y <> 15 then
          Error (format "Expected y positions [0,5,15], got [%d,%d,%d]"
            rect1.y rect2.y rect3.y)
        else
          Ok ()
  | _ -> Error "Expected Container node"

(** Test 8: Column with flex-height children *)
let test_column_flex_heights () =
  let elem = E.column [
    E.box ~style:(S.default |> S.height_flex 1.0) (E.text "A");
    E.box ~style:(S.default |> S.height_flex 3.0) (E.text "B");
    E.box ~style:(S.default |> S.height_flex 1.0) (E.text "C");
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:20 ~height:50 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      let rects = List.map get_rect children in
      let heights = List.map (fun r -> r.Scene.height) rects in
      (* Total flex = 5.0, available = 50 *)
      (* Child A: 50 * (1/5) = 10 *)
      (* Child B: 50 * (3/5) = 30 *)
      (* Child C: 50 * (1/5) = 10 *)
      if heights = [10; 30; 10] then
        Ok ()
      else
        Error (format "Expected heights [10,30,10], got [%d;%d;%d]"
          (List.nth heights 0) (List.nth heights 1) (List.nth heights 2))
  | _ -> Error "Expected Container node"

(** Test 9: Nested column with row *)
let test_nested_column_row () =
  let elem = E.column [
    E.box ~style:(S.default |> S.height_fixed 10) (E.text "Header");
    E.row [
      E.box ~style:(S.default |> S.width_flex 1.0) (E.text "Left");
      E.box ~style:(S.default |> S.width_flex 1.0) (E.text "Right");
    ];
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:40 ~height:50 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      if List.length children <> 2 then
        Error (format "Expected 2 children, got %d" (List.length children))
      else
        let header = List.nth children 0 in
        let row = List.nth children 1 in
        
        (* Check header *)
        let header_rect = get_rect header in
        if header_rect.height <> 10 then
          Error (format "Header height should be 10, got %d" header_rect.height)
        else
          (* Check row children *)
          match row.content with
          | Scene.Container { children = row_children; _ } ->
              let row_rects = List.map get_rect row_children in
              let widths = List.map (fun r -> r.Scene.width) row_rects in
              if widths = [20; 20] then
                Ok ()
              else
                Error (format "Row children widths should be [20,20], got [%d;%d]"
                  (List.nth widths 0) (List.nth widths 1))
          | _ -> Error "Expected row to be Container"
  | _ -> Error "Expected Container node"

(** Test 10: Box with background style is preserved in scene *)
let test_box_with_background () =
  let blue = S.color "#0000FF" in
  let elem = E.box 
    ~style:(S.default |> S.bg blue)
    (E.empty) 
  in
  let ctx = make_ctx ~x:0 ~y:0 ~width:10 ~height:5 in
  let scene = Layout.to_scene elem ctx in
  
  match get_style scene with
  | Some style ->
      if style.Scene.bg = Some blue then
        Ok ()
      else
        Error "Background color not preserved"
  | None -> Error "Container should have style"

(** Test 11: Row with mixed fixed and flex widths *)
let test_row_mixed_sizing () =
  let elem = E.row [
    E.box ~style:(S.default |> S.width_fixed 10) (E.text "A");
    E.box ~style:(S.default |> S.width_flex 1.0) (E.text "B");
    E.box ~style:(S.default |> S.width_fixed 5) (E.text "C");
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:50 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      let widths = List.map (fun c -> (get_rect c).Scene.width) children in
      (* Total = 50, Fixed = 10 + 5 = 15, Remaining = 35 for flex *)
      if widths = [10; 35; 5] then Ok ()
      else Error (format "Expected widths [10,35,5], got %d items" (List.length widths))
  | _ -> Error "Expected Container node"

(** Test 12: Column with mixed fixed and flex heights *)
let test_column_mixed_sizing () =
  let elem = E.column [
    E.box ~style:(S.default |> S.height_fixed 8) (E.text "A");
    E.box ~style:(S.default |> S.height_flex 1.0) (E.text "B");
    E.box ~style:(S.default |> S.height_fixed 12) (E.text "C");
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:20 ~height:50 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      let heights = List.map (fun c -> (get_rect c).Scene.height) children in
      (* Total = 50, Fixed = 8 + 12 = 20, Remaining = 30 for flex *)
      if heights = [8; 30; 12] then Ok ()
      else Error "Heights don't match expected"
  | _ -> Error "Expected Container node"

(** Test 13: Empty row *)
let test_empty_row () =
  let elem = E.row [] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:20 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      if List.length children = 0 then Ok ()
      else Error "Expected 0 children"
  | _ -> Error "Expected Container node"

(** Test 14: Row with single flex child uses full width *)
let test_row_single_flex () =
  let elem = E.row [
    E.box ~style:(S.default |> S.width_flex 1.0) (E.text "Single");
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:100 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      if List.length children <> 1 then Error "Should have 1 child"
      else
        let rect = get_rect (List.hd children) in
        if rect.width = 100 then Ok ()
        else Error (format "Expected width 100, got %d" rect.width)
  | _ -> Error "Expected Container node"

(** Test 15: Row with unequal flex weights *)
let test_row_unequal_flex () =
  let elem = E.row [
    E.box ~style:(S.default |> S.width_flex 1.0) (E.text "A");
    E.box ~style:(S.default |> S.width_flex 3.0) (E.text "B");
    E.box ~style:(S.default |> S.width_flex 2.0) (E.text "C");
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:60 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      let widths = List.map (fun c -> (get_rect c).Scene.width) children in
      (* Total flex = 6.0, widths = [10, 30, 20] *)
      if widths = [10; 30; 20] then Ok ()
      else Error "Unequal flex distribution wrong"
  | _ -> Error "Expected Container node"

(** Test 16: Box with all padding directions *)
let test_box_all_padding () =
  let elem = E.box 
    ~style:(S.default 
      |> S.padding_left 5
      |> S.padding_right 7
      |> S.padding_top 3
      |> S.padding_bottom 4)
    (E.text "X") 
  in
  let ctx = make_ctx ~x:10 ~y:10 ~width:30 ~height:20 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      if List.length children = 0 then Error "Should have child"
      else
        let child_rect = get_rect (List.hd children) in
        (* Child x = 10 + 5 = 15, y = 10 + 3 = 13 *)
        (* Child width = 30 - 5 - 7 = 18, height = 20 - 3 - 4 = 13 *)
        if child_rect.x = 15 && child_rect.y = 13 && 
           child_rect.width = 18 && child_rect.height = 13 then Ok ()
        else Error "Padding calculation wrong"
  | _ -> Error "Expected Container node"

(** Test 17: Row positions children correctly *)
let test_row_child_positions () =
  let elem = E.row [
    E.box ~style:(S.default |> S.width_fixed 10) (E.text "A");
    E.box ~style:(S.default |> S.width_fixed 15) (E.text "B");
    E.box ~style:(S.default |> S.width_fixed 12) (E.text "C");
  ] in
  let ctx = make_ctx ~x:5 ~y:8 ~width:100 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      let xs = List.map (fun c -> (get_rect c).Scene.x) children in
      if xs = [5; 15; 30] then Ok ()
      else Error "Row X positions wrong"
  | _ -> Error "Expected Container node"

(** Test 18: Column positions children correctly *)
let test_column_child_positions () =
  let elem = E.column [
    E.box ~style:(S.default |> S.height_fixed 8) (E.text "A");
    E.box ~style:(S.default |> S.height_fixed 12) (E.text "B");
    E.box ~style:(S.default |> S.height_fixed 5) (E.text "C");
  ] in
  let ctx = make_ctx ~x:10 ~y:20 ~width:30 ~height:100 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      let ys = List.map (fun c -> (get_rect c).Scene.y) children in
      if ys = [20; 28; 40] then Ok ()
      else Error "Column Y positions wrong"
  | _ -> Error "Expected Container node"

(** Test 19: Box with zero dimensions *)
let test_box_zero_dimensions () =
  let elem = E.box (E.text "X") in
  let ctx = make_ctx ~x:0 ~y:0 ~width:0 ~height:0 in
  let scene = Layout.to_scene elem ctx in
  
  let rect = get_rect scene in
  if rect.width = 0 && rect.height = 0 then Ok ()
  else Error "Zero dimensions not preserved"

(** Test 20: Box preserves foreground color *)
let test_box_with_foreground () =
  let red = S.color "#FF0000" in
  let elem = E.box ~style:(S.default |> S.fg red) (E.empty) in
  let ctx = make_ctx ~x:0 ~y:0 ~width:10 ~height:5 in
  let scene = Layout.to_scene elem ctx in
  
  match get_style scene with
  | Some style ->
      if style.Scene.fg = Some red then Ok ()
      else Error "Foreground color not preserved"
  | None -> Error "Should have style"

(** Test 21: Box with bold style *)
let test_box_with_bold () =
  let elem = E.box ~style:(S.default |> S.bold true) (E.empty) in
  let ctx = make_ctx ~x:0 ~y:0 ~width:10 ~height:5 in
  let scene = Layout.to_scene elem ctx in
  
  match get_style scene with
  | Some style ->
      if style.Scene.bold then Ok ()
      else Error "Bold not preserved"
  | None -> Error "Should have style"

(** Test 22: Box with italic style *)
let test_box_with_italic () =
  let elem = E.box ~style:(S.default |> S.italic true) (E.empty) in
  let ctx = make_ctx ~x:0 ~y:0 ~width:10 ~height:5 in
  let scene = Layout.to_scene elem ctx in
  
  match get_style scene with
  | Some style ->
      if style.Scene.italic then Ok ()
      else Error "Italic not preserved"
  | None -> Error "Should have style"

(** Test 23: Box with underline style *)
let test_box_with_underline () =
  let elem = E.box ~style:(S.default |> S.underline true) (E.empty) in
  let ctx = make_ctx ~x:0 ~y:0 ~width:10 ~height:5 in
  let scene = Layout.to_scene elem ctx in
  
  match get_style scene with
  | Some style ->
      if style.Scene.underline then Ok ()
      else Error "Underline not preserved"
  | None -> Error "Should have style"

(** Test 24: Deeply nested structure *)
let test_deeply_nested () =
  let elem = E.column [
    E.row [
      E.column [
        E.box (E.text "Deep");
      ];
    ];
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:50 ~height:50 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container _ -> Ok ()
  | _ -> Error "Expected Container"

(** Test 25: Text with multiline content *)
let test_text_multiline () =
  let elem = E.text "Line1\nLine2\nLine3" in
  let ctx = make_ctx ~x:0 ~y:0 ~width:20 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.TextNode { text; _ } ->
      if text = "Line1\nLine2\nLine3" then Ok ()
      else Error "Text content wrong"
  | _ -> Error "Expected TextNode"

(** Test 26: Text with padding offset *)
let test_text_with_padding () =
  let elem = E.text 
    ~style:(S.default 
      |> S.padding_left 3
      |> S.padding_top 2)
    "Test" 
  in
  let ctx = make_ctx ~x:10 ~y:5 ~width:20 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  let rect = get_rect scene in
  if rect.x = 13 && rect.y = 7 then Ok ()
  else Error "Text padding offset wrong"

(** Test 27: Column with all flex heights equal *)
let test_column_equal_flex () =
  let elem = E.column [
    E.box ~style:(S.default |> S.height_flex 1.0) (E.text "A");
    E.box ~style:(S.default |> S.height_flex 1.0) (E.text "B");
    E.box ~style:(S.default |> S.height_flex 1.0) (E.text "C");
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:20 ~height:60 in
  let scene = Layout.to_scene elem ctx in
  
  match scene.content with
  | Scene.Container { children; _ } ->
      let heights = List.map (fun c -> (get_rect c).Scene.height) children in
      if heights = [20; 20; 20] then Ok ()
      else Error "Equal flex heights wrong"
  | _ -> Error "Expected Container"

(** Test 28: Row exceeding available width *)
let test_row_overflow () =
  let elem = E.row [
    E.box ~style:(S.default |> S.width_fixed 60) (E.text "A");
    E.box ~style:(S.default |> S.width_fixed 60) (E.text "B");
  ] in
  let ctx = make_ctx ~x:0 ~y:0 ~width:100 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  (* Should still layout, just overflow *)
  match scene.content with
  | Scene.Container { children; _ } ->
      if List.length children = 2 then Ok ()
      else Error "Should have 2 children"
  | _ -> Error "Expected Container"

(** Test 29: Empty element *)
let test_empty_element () =
  let elem = E.empty in
  let ctx = make_ctx ~x:0 ~y:0 ~width:10 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  let rect = get_rect scene in
  if rect.width = 0 && rect.height = 0 then Ok ()
  else Error "Empty should have zero size"

(** Test 30: Box within box with padding *)
let test_nested_box_padding () =
  let elem = E.box
    ~style:(S.default |> S.padding_left 5)
    (E.box
      ~style:(S.default |> S.padding_left 3)
      (E.text "X"))
  in
  let ctx = make_ctx ~x:0 ~y:0 ~width:20 ~height:10 in
  let scene = Layout.to_scene elem ctx in
  
  (* Verify structure exists *)
  match scene.content with
  | Scene.Container _ -> Ok ()
  | _ -> Error "Expected Container"

(** Test 31: Blue box full screen - flex 1.0 with blue background *)
let test_blue_box_full_screen () =
  let blue = S.color "#0000FF" in
  let elem = E.box
    ~style:(S.default
      |> S.width_flex 1.0
      |> S.height_flex 1.0
      |> S.bg blue)
    (E.text "")
  in
  let ctx = make_ctx ~x:0 ~y:0 ~width:40 ~height:50 in
  let scene = Layout.to_scene elem ctx in
  
  let rect = get_rect scene in
  (* Should fill entire available space *)
  if rect.x <> 0 || rect.y <> 0 || rect.width <> 40 || rect.height <> 50 then
    Error (format "Expected rect (0,0,40x50), got (%d,%d,%dx%d)"
      rect.x rect.y rect.width rect.height)
  else
    (* Verify it's a container with blue background *)
    match scene.content with
    | Scene.Container { style = Some s; _ } ->
        if Option.is_some s.Scene.bg then Ok ()
        else Error "Expected background color"
    | Scene.Container { style = None; _ } ->
        Error "Expected style with background"
    | _ -> Error "Expected Container node"

let tests =
  Test.[
    case "box creates container" test_box_creates_container;
    case "box with fixed size" test_box_fixed_size;
    case "box with padding" test_box_with_padding;
    case "text creates text node" test_text_creates_text_node;
    case "row with fixed widths" test_row_fixed_widths;
    case "row with flex widths" test_row_flex_widths;
    case "column with fixed heights" test_column_fixed_heights;
    case "column with flex heights" test_column_flex_heights;
    case "nested column and row" test_nested_column_row;
    case "box with background style" test_box_with_background;
    case "row mixed sizing" test_row_mixed_sizing;
    case "column mixed sizing" test_column_mixed_sizing;
    case "empty row" test_empty_row;
    case "row single flex" test_row_single_flex;
    case "row unequal flex" test_row_unequal_flex;
    case "box all padding" test_box_all_padding;
    case "row child positions" test_row_child_positions;
    case "column child positions" test_column_child_positions;
    case "box zero dimensions" test_box_zero_dimensions;
    case "box with foreground" test_box_with_foreground;
    case "box with bold" test_box_with_bold;
    case "box with italic" test_box_with_italic;
    case "box with underline" test_box_with_underline;
    case "deeply nested" test_deeply_nested;
    case "text multiline" test_text_multiline;
    case "text with padding" test_text_with_padding;
    case "column equal flex" test_column_equal_flex;
    case "row overflow" test_row_overflow;
    case "empty element" test_empty_element;
    case "nested box padding" test_nested_box_padding;
    case "blue box full screen" test_blue_box_full_screen;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"layout-to-scene" ~tests ~args)
    ~args:Env.args ()
