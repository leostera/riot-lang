open Std
open Std.Collections

type layout_node = {
  element: Element.t;
  style: Style.t;
  children: layout_node list;
  mutable computed_size: Viewport.t;
  mutable computed_position: Geometry.Point.t;
  mutable final_box: Geometry.Rect.t;
}

(* Helper: Get style from element *)

let get_element_style =
  function
  | Element.Text { style; _ } -> style
  | Element.Container { style; _ } -> style
  | Element.Empty -> Style.empty
  | Element.Custom { style; _ } -> style

(* Phase 1: Build layout tree from element tree *)

let rec build_layout_tree : Element.t -> layout_node = fun element ->
  let style = get_element_style element in
  let children =
    match element with
    | Element.Container { children; _ } -> List.map build_layout_tree children
    | Element.Text _
    | Element.Empty
    | Element.Custom _ -> []
  in
  {
    element;
    style;
    children;
    computed_size = Viewport.make ~width:0.0 ~height:0.0;
    computed_position = Geometry.Point.zero;
    final_box = Geometry.Rect.zero;

  }

(* Helper: Calculate padding dimensions *)

let padding_horizontal = fun (p:Style.padding) -> Float.of_int (p.left + p.right)

let padding_vertical = fun (p:Style.padding) -> Float.of_int (p.top + p.bottom)

(* Helper: Clamp value between min and max *)

let clamp_option = fun value min_opt max_opt ->
  let v =
    match min_opt with
    | Some min -> Float.max value min
    | None -> value
  in
  match max_opt with
  | Some max -> Float.min v max
  | None -> v

(* Phase 2: Calculate sizes (ported from Clay) *)

let rec calculate_sizes : layout_node -> Viewport.t -> Super.Config.t -> unit = fun node available config ->
  let style = node.style in
  (* Calculate intrinsic size based on content *)
  let intrinsic_size =
    match node.element with
    | Element.Text { content; _ } -> config.text_measurer content style
    | Element.Container _ ->
        if style.sizing.width = Style.Fit || style.sizing.height = Style.Fit then
          calculate_container_fit_size node available config
        else
          Viewport.make ~width:0.0 ~height:0.0
    | Element.Empty -> Viewport.make ~width:0.0 ~height:0.0
    | Element.Custom { measure; _ } -> measure ()
  in
  (* Apply sizing rules *)
  let calculated_width =
    match style.sizing.width with
    | Style.Fit -> intrinsic_size.width +. padding_horizontal style.padding
    | Style.Grow -> available.width
    | Style.Fixed w -> w
    | Style.Percent p -> available.width *. p
  in
  let calculated_height =
    match style.sizing.height with
    | Style.Fit -> intrinsic_size.height +. padding_vertical style.padding
    | Style.Grow -> available.height
    | Style.Fixed h -> h
    | Style.Percent p -> available.height *. p
  in
  (* Apply min/max constraints *)
  let final_width = clamp_option calculated_width style.sizing.min_width style.sizing.max_width in
  let final_height = clamp_option calculated_height style.sizing.min_height style.sizing.max_height in
  node.computed_size <- Viewport.make ~width:final_width ~height:final_height;
  (* Layout children if this is a container *)
  match node.element with
  | Element.Container _ -> layout_children node config
  | _ -> ()
and calculate_container_fit_size : layout_node -> Viewport.t -> Super.Config.t -> Viewport.t = fun node available config ->
  let style = node.style in
  (* First calculate all children sizes *)
  List.iter (fun child -> calculate_sizes child available config) node.children;
  (* Sum up children based on direction *)
  let total_width, total_height =
    List.fold_left
      (fun ((w, h)) child ->
        match style.direction with
        | Style.LeftToRight -> (
          w +. child.computed_size.width,
          Float.max h child.computed_size.height
        )
        | Style.TopToBottom -> (
          Float.max w child.computed_size.width,
          h +. child.computed_size.height
        ))
      (0.0, 0.0)
      node.children
  in
  (* Add gaps between children *)
  let child_count = List.length node.children in
  let gap_space = Float.of_int (style.child_gap * (child_count - 1)) in
  let width =
    match style.direction with
    | Style.LeftToRight -> total_width +. gap_space
    | Style.TopToBottom -> total_width
  in
  let height =
    match style.direction with
    | Style.LeftToRight -> total_height
    | Style.TopToBottom -> total_height +. gap_space
  in
  Viewport.make ~width ~height
and layout_children : layout_node -> Super.Config.t -> unit = fun node config ->
  let style = node.style in
  let available_width = node.computed_size.width -. padding_horizontal style.padding in
  let available_height = node.computed_size.height -. padding_vertical style.padding in
  (* Partition children by sizing type *)
  let fit_children, grow_children, fixed_children, percent_children =
    List.fold_left
      (fun ((fit, grow, fixed, percent)) child ->
        let child_style = child.style in
        match style.direction with
        | Style.LeftToRight -> (
            match child_style.sizing.width with
            | Style.Fit -> (child :: fit, grow, fixed, percent)
            | Style.Grow -> (fit, child :: grow, fixed, percent)
            | Style.Fixed _ -> (fit, grow, child :: fixed, percent)
            | Style.Percent _ -> (fit, grow, fixed, child :: percent)
          )
        | Style.TopToBottom -> (
            match child_style.sizing.height with
            | Style.Fit -> (child :: fit, grow, fixed, percent)
            | Style.Grow -> (fit, child :: grow, fixed, percent)
            | Style.Fixed _ -> (fit, grow, child :: fixed, percent)
            | Style.Percent _ -> (fit, grow, fixed, child :: percent)
          ))
      ([], [], [], [])
      node.children
  in
  (* Calculate space used by FIT and FIXED children *)
  let fit_space =
    List.fold_left
      (fun acc child ->
        acc +. (
          match style.direction with
          | Style.LeftToRight -> child.computed_size.width
          | Style.TopToBottom -> child.computed_size.height
        ))
      0.0
      fit_children
  in
  let fixed_space =
    List.fold_left
      (fun acc child ->
        let size =
          match style.direction with
          | Style.LeftToRight -> (
              match child.style.sizing.width with
              | Style.Fixed w -> w
              | _ -> 0.0
            )
          | Style.TopToBottom -> (
              match child.style.sizing.height with
              | Style.Fixed h -> h
              | _ -> 0.0
            )
        in
        acc +. size)
      0.0
      fixed_children
  in
  let percent_space =
    List.fold_left
      (fun acc child ->
        let size =
          match style.direction with
          | Style.LeftToRight -> (
              match child.style.sizing.width with
              | Style.Percent p -> available_width *. p
              | _ -> 0.0
            )
          | Style.TopToBottom -> (
              match child.style.sizing.height with
              | Style.Percent p -> available_height *. p
              | _ -> 0.0
            )
        in
        acc +. size)
      0.0
      percent_children
  in
  (* Calculate gap space *)
  let child_count = List.length node.children in
  let gap_space = Float.of_int (style.child_gap * (child_count - 1)) in
  (* Remaining space for GROW children *)
  let total_available =
    match style.direction with
    | Style.LeftToRight -> available_width
    | Style.TopToBottom -> available_height
  in
  let remaining_space = Float.max
  0.0
  (total_available -. fit_space -. fixed_space -. percent_space -. gap_space) in
  (* Distribute to GROW children *)
  let grow_count = List.length grow_children in
  let space_per_grow =
    if grow_count > 0 then
      remaining_space /. Float.of_int grow_count
    else
      0.0
  in
  List.iter
    (fun child ->
      match style.direction with
      | Style.LeftToRight -> child.computed_size <- Viewport.make ~width:space_per_grow ~height:available_height
      | Style.TopToBottom -> child.computed_size <- Viewport.make ~width:available_width ~height:space_per_grow)
    grow_children

(* Phase 3: Calculate positions *)

let rec calculate_positions : layout_node -> Geometry.Point.t -> unit = fun node origin ->
  let style = node.style in
  node.computed_position <- origin;
  (* Calculate content origin (inside padding) *)
  let content_origin = Geometry.Point.make
  ~x:((((origin.x +. Float.of_int style.padding.left))))
  ~y:((((origin.y +. Float.of_int style.padding.top)))) in
  (* Set final bounding box *)
  node.final_box <- Geometry.Rect.make
  ~x:origin.x
  ~y:origin.y
  ~width:node.computed_size.width
  ~height:node.computed_size.height;
  (* Position children *)
  let next_x = cell content_origin.x in
  let next_y = cell content_origin.y in
  List.iter
    (fun child ->
      (* TODO: Apply alignment here *)
      let child_pos = Geometry.Point.make ~x:!next_x ~y:!next_y in
      calculate_positions child child_pos;
      (* Update next position *)
      match style.direction with
      | Style.LeftToRight -> next_x := !next_x +. child.computed_size.width +. Float.of_int style.child_gap
      | Style.TopToBottom -> next_y := !next_y +. child.computed_size.height +. Float.of_int style.child_gap)
    node.children

(* Phase 4: Generate render commands *)

let rec generate_commands : layout_node -> Render.command Vector.t -> unit = fun node commands ->
  let style = node.style in
  (* Generate background rectangle *)
  (
    match style.background with
    | Some color ->
        let cmd = {
          Render.bounding_box = node.final_box;
          command_type = Render.Rectangle {color; corner_radius = style.corner_radius; };
          z_index = style.z_index;

        } in
        Vector.push commands cmd
    | None -> ()
  );
  (* Generate border *)
  (
    if style.border_width > 0 then
      match style.border_color with
      | Some color ->
          let cmd = {
            Render.bounding_box = node.final_box;
            command_type = Render.Border {
              width = {
                left = style.border_width;
                right = style.border_width;
                top = style.border_width;
                bottom = style.border_width;

              };
              color;
              corner_radius = style.corner_radius;

            };
            z_index = style.z_index;

          } in
          Vector.push commands cmd
      | None -> ()
  );
  (* Generate element-specific commands *)
  (
    match node.element with
    | Element.Text { content; _ } ->
        let text_color = Option.unwrap_or style.foreground ~default:(`rgb (255, 255, 255)) in
        let cmd = {
          Render.bounding_box = node.final_box;
          command_type = Render.Text {
            content;
            color = text_color;
            size = style.text_size;
            weight = style.font_weight;

          };
          z_index = style.z_index;

        } in
        Vector.push commands cmd
    | Element.Container _ ->
        (* Recurse into children *)
        List.iter (fun child -> generate_commands child commands) node.children
    | Element.Custom { render; _ } ->
        let custom_commands = render node.final_box in
        List.iter (Vector.push commands) custom_commands
    | Element.Empty ->
        ()
  );
  ()

(* Main compute function *)

let compute = fun ~config element ->
  (* Build layout tree *)
  let layout_tree = build_layout_tree element in
  (* Calculate sizes *)
  calculate_sizes layout_tree config.Super.Config.viewport config;
  (* Calculate positions *)
  calculate_positions layout_tree Geometry.Point.zero;
  (* Generate render commands *)
  let commands = Vector.create () in
  generate_commands layout_tree commands;
  (* Sort by z-index and convert to list *)
  let cmd_list = commands |> Vector.into_iter |> Iter.Iterator.to_list in
  List.sort
    (fun a b ->
      Int.compare a.Render.z_index b.Render.z_index)
    cmd_list
