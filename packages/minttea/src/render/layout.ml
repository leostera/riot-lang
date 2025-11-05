(** Layout - Convert Element tree to positioned Scene graph *)

open Std

(** Layout context - tracks position and available space *)
type ctx = {
  x : int;
  y : int;
  available_width : int;
  available_height : int;
}

(** Helper: Extract style from element *)
let get_style = function
  | Element.Text (s, _) | Element.Box (s, _) | Element.Row (s, _) 
  | Element.Column (s, _) | Element.Spacer s | Element.Empty s -> s

(** Helper: Measure intrinsic width *)
let rec measure_width = function
  | Element.Text (style, str) ->
      let lines = Util.Ansi.split_lines str in
      let content_width = List.fold_left (fun acc line ->
        Int.max acc (Util.Ansi.width line)
      ) 0 lines in
      (* Add padding *)
      let padding_h = Style.get_padding_left style + Style.get_padding_right style in
      content_width + padding_h
      
  | Element.Box (style, child) ->
      let child_width = measure_width child in
      let padding_h = Style.get_padding_left style + Style.get_padding_right style in
      child_width + padding_h
      
  | Element.Row (style, children) ->
      let children_width = List.fold_left (fun acc child ->
        acc + measure_width child
      ) 0 children in
      let padding_h = Style.get_padding_left style + Style.get_padding_right style in
      children_width + padding_h
      
  | Element.Column (style, children) ->
      let max_child_width = List.fold_left (fun acc child ->
        Int.max acc (measure_width child)
      ) 0 children in
      let padding_h = Style.get_padding_left style + Style.get_padding_right style in
      max_child_width + padding_h
      
  | Element.Spacer _ -> 0
  | Element.Empty _ -> 0

(** Helper: Measure intrinsic height *)
let rec measure_height = function
  | Element.Text (style, str) ->
      let lines = Util.Ansi.split_lines str in
      let content_height = List.length lines in
      let padding_v = Style.get_padding_top style + Style.get_padding_bottom style in
      content_height + padding_v
      
  | Element.Box (style, child) ->
      let child_height = measure_height child in
      let padding_v = Style.get_padding_top style + Style.get_padding_bottom style in
      child_height + padding_v
      
  | Element.Row (style, children) ->
      let max_child_height = List.fold_left (fun acc child ->
        Int.max acc (measure_height child)
      ) 0 children in
      let padding_v = Style.get_padding_top style + Style.get_padding_bottom style in
      max_child_height + padding_v
      
  | Element.Column (style, children) ->
      let children_height = List.fold_left (fun acc child ->
        acc + measure_height child
      ) 0 children in
      let padding_v = Style.get_padding_top style + Style.get_padding_bottom style in
      children_height + padding_v
      
  | Element.Spacer _ -> 0
  | Element.Empty _ -> 0

(** Resolve sizes along a single axis *)
let resolve_axis (specs : (Style.size * int) list) (available : int) : int list =
  (* Step 1: Sum up Fixed sizes *)
  let fixed_total = 
    List.fold_left (fun acc (size, _) ->
      match size with
      | Style.Fixed n -> acc + n
      | _ -> acc
    ) 0 specs
  in
  
  (* Step 2: Sum up Auto sizes (already measured) *)
  let auto_total =
    List.fold_left (fun acc (size, measured) ->
      match size with
      | Style.Auto -> acc + measured
      | _ -> acc
    ) 0 specs
  in
  
  (* Step 3: Calculate remaining space for Flex *)
  let remaining = Int.max 0 (available - fixed_total - auto_total) in
  
  (* Step 4: Sum up Flex weights *)
  let flex_total =
    List.fold_left (fun acc (size, _) ->
      match size with
      | Style.Flex weight -> acc +. weight
      | _ -> acc
    ) 0.0 specs
  in
  
  (* Step 5: Assign final sizes *)
  List.map (fun (size, measured) ->
    match size with
    | Style.Fixed n -> n
    | Style.Auto -> measured
    | Style.Flex weight ->
        if flex_total > 0.0 then
          int_of_float (float_of_int remaining *. (weight /. flex_total))
        else 0
  ) specs

(** Helper: Convert Style to Scene style attributes *)
let style_to_scene_attrs style =
  Scene.{
    fg = Style.get_foreground style;
    bg = Style.get_background style;
    bold = Style.get_bold style;
    italic = Style.get_italic style;
    underline = Style.get_underline style;
    strikethrough = Style.get_strikethrough style;
    reverse = Style.get_reverse style;
  }

(** Layout an element tree into a positioned Scene graph *)
let rec to_scene element ctx =
  let style = get_style element in
  let padding_left = Style.get_padding_left style in
  let padding_right = Style.get_padding_right style in
  let padding_top = Style.get_padding_top style in
  let padding_bottom = Style.get_padding_bottom style in
  
  (* Calculate content area (after padding) *)
  let content_width = Int.max 0 (ctx.available_width - padding_left - padding_right) in
  let content_height = Int.max 0 (ctx.available_height - padding_top - padding_bottom) in
  
  match element with
  | Element.Text (style, text) ->
      let scene_style = style_to_scene_attrs style in
      let rect = Scene.{
        x = ctx.x + padding_left;
        y = ctx.y + padding_top;
        width = content_width;
        height = content_height;
      } in
      Scene.text_node ~rect ~z_index:0 ~style:scene_style text
      
  | Element.Box (style, child) ->
      let child_ctx = {
        x = ctx.x + padding_left;
        y = ctx.y + padding_top;
        available_width = content_width;
        available_height = content_height;
      } in
      let child_node = to_scene child child_ctx in
      let rect = Scene.{x = ctx.x; y = ctx.y; width = ctx.available_width; height = ctx.available_height} in
      let scene_style = style_to_scene_attrs style in
      Scene.container ~rect ~z_index:0 ~style:scene_style [child_node]
      
  | Element.Row (style, children) ->
      layout_row style children ctx content_width content_height
      
  | Element.Column (style, children) ->
      layout_column style children ctx content_width content_height
      
  | Element.Spacer _ | Element.Empty _ ->
      let rect = Scene.{x = ctx.x; y = ctx.y; width = 0; height = 0} in
      Scene.container ~rect ~z_index:0 []

(** Layout children in a row (horizontal) *)
and layout_row style children ctx content_width content_height =
  let padding_left = Style.get_padding_left style in
  let padding_top = Style.get_padding_top style in
  
  (* Collect width specs and measurements *)
  let specs = List.map (fun child ->
    let child_style = get_style child in
    (Style.get_width child_style, measure_width child)
  ) children in
  
  (* Resolve widths using existing axis resolution *)
  let widths = resolve_axis specs content_width in
  
  (* Layout each child *)
  let child_nodes = ref [] in
  let current_x = ref (ctx.x + padding_left) in
  
  List.iter2 (fun child width ->
    let child_ctx = {
      x = !current_x;
      y = ctx.y + padding_top;
      available_width = width;
      available_height = content_height;
    } in
    let child_node = to_scene child child_ctx in
    child_nodes := child_node :: !child_nodes;
    current_x := !current_x + width;
  ) children widths;
  
  let rect = Scene.{x = ctx.x; y = ctx.y; width = ctx.available_width; height = ctx.available_height} in
  let scene_style = style_to_scene_attrs style in
  Scene.container ~rect ~z_index:0 ~style:scene_style (List.rev !child_nodes)

(** Layout children in a column (vertical) *)
and layout_column style children ctx content_width content_height =
  let padding_left = Style.get_padding_left style in
  let padding_top = Style.get_padding_top style in
  
  (* Collect height specs and measurements *)
  let specs = List.map (fun child ->
    let child_style = get_style child in
    (Style.get_height child_style, measure_height child)
  ) children in
  
  (* Resolve heights using existing axis resolution *)
  let heights = resolve_axis specs content_height in
  
  (* Layout each child *)
  let child_nodes = ref [] in
  let current_y = ref (ctx.y + padding_top) in
  
  List.iter2 (fun child height ->
    let child_ctx = {
      x = ctx.x + padding_left;
      y = !current_y;
      available_width = content_width;
      available_height = height;
    } in
    let child_node = to_scene child child_ctx in
    child_nodes := child_node :: !child_nodes;
    current_y := !current_y + height;
  ) children heights;
  
  let rect = Scene.{x = ctx.x; y = ctx.y; width = ctx.available_width; height = ctx.available_height} in
  let scene_style = style_to_scene_attrs style in
  Scene.container ~rect ~z_index:0 ~style:scene_style (List.rev !child_nodes)
