open Std
open Std.Collections

type layout_node = {
  element: Element.t;
  style: Style.t;
  children: layout_node list;
  mutable computed_size: Viewport.t;
  mutable final_box: Geometry.Rect.t;
  mutable measured_text: Super.Config.text_measurement option;
}

type node_constraints = {
  max_width: float option;
  max_height: float option;
  forced_width: float option;
  forced_height: float option;
}

type clipped_command = {
  command: Render.command;
  clip_stack: Geometry.Rect.t list;
}

let zero_viewport = Viewport.make ~width:0.0 ~height:0.0

let empty_node_constraints = {
  max_width = None;
  max_height = None;
  forced_width = None;
  forced_height = None;
}

module Math = struct
  let map_option = fun value fn ->
    match value with
    | Some value -> Some (fn value)
    | None -> None

  let float_max = fun left right ->
    match Float.compare left right with
    | Order.LT -> right
    | Order.EQ
    | Order.GT -> left

  let float_min = fun left right ->
    match Float.compare left right with
    | Order.LT
    | Order.EQ -> left
    | Order.GT -> right

  let clamp_non_negative = fun value ->
    if (
      match Float.compare value 0.0 with
      | Order.LT -> true
      | Order.EQ
      | Order.GT -> false
    ) then
      0.0
    else
      value

  let clamp_between = fun value min_opt max_opt ->
    let value =
      match min_opt with
      | Some min -> float_max value min
      | None -> value
    in
    match max_opt with
    | Some max -> float_min value max
    | None -> value

  let option_subtract = fun value delta ->
    map_option
      value
      (fun value -> clamp_non_negative (value -. delta))

  let option_default = fun value default ->
    match value with
    | Some value -> value
    | None -> default

  let list_last_index = fun values ->
    let len = List.length values in
    Int.max 0 (len - 1)
end

module Box_model = struct
  let border_thickness = fun (style: Style.t) ->
    if style.border_width > 0 then
      1
    else
      0

  let horizontal_inset = fun (style: Style.t) ->
    let border = border_thickness style in
    Float.from_int (style.padding.left + style.padding.right + border + border)

  let vertical_inset = fun (style: Style.t) ->
    let border = border_thickness style in
    Float.from_int (style.padding.top + style.padding.bottom + border + border)

  let content_origin = fun (rect: Geometry.Rect.t) (style: Style.t) ->
    let border = Float.from_int (border_thickness style) in
    Geometry.Point.make
      ~x:(rect.x +. border +. Float.from_int style.padding.left)
      ~y:(rect.y +. border +. Float.from_int style.padding.top)

  let content_box = fun (rect: Geometry.Rect.t) (style: Style.t) ->
    let inset_x = horizontal_inset style in
    let inset_y = vertical_inset style in
    let origin = content_origin rect style in
    Geometry.Rect.make
      ~x:origin.x
      ~y:origin.y
      ~width:(Math.clamp_non_negative (rect.width -. inset_x))
      ~height:(Math.clamp_non_negative (rect.height -. inset_y))

  let margin_horizontal = fun (margin: Style.margin) -> Float.from_int (margin.left + margin.right)

  let margin_vertical = fun (margin: Style.margin) -> Float.from_int (margin.top + margin.bottom)

  let outer_width = fun node -> node.computed_size.width +. margin_horizontal node.style.margin

  let outer_height = fun node -> node.computed_size.height +. margin_vertical node.style.margin
end

let measurement_constraints = fun ?available_width ?available_height () ->
  Super.Config.constraints
    ?available_width
    ?available_height
    ()

let get_element_style = fun __tmp1 ->
  match __tmp1 with
  | Element.Text { style; _ } -> style
  | Element.Container { style; _ } -> style
  | Element.Empty -> Style.empty
  | Element.Custom { style; _ } -> style

let rec build_layout_tree: Element.t -> layout_node = fun element ->
  let style = get_element_style element in
  let children =
    match element with
    | Element.Container { children; _ } -> List.map children ~fn:build_layout_tree
    | Element.Text _
    | Element.Empty
    | Element.Custom _ -> []
  in
  {
    element;
    style;
    children;
    computed_size = zero_viewport;
    final_box = Geometry.Rect.zero;
    measured_text = None;
  }

let resolve_non_fit_size = fun sizing available ->
  match sizing with
  | Style.Fixed value -> Some value
  | Style.Percent ratio -> Math.map_option available (fun value -> value *. ratio)
  | Style.Grow -> available
  | Style.Fit -> None

let resolve_axis_size = fun sizing intrinsic available forced min_opt max_opt ->
  let value =
    match forced with
    | Some value -> value
    | None -> (
        match sizing with
        | Style.Fit -> intrinsic
        | Style.Fixed value -> value
        | Style.Percent ratio ->
            Math.option_default (Math.map_option available (fun value -> value *. ratio)) intrinsic
        | Style.Grow -> Math.option_default available intrinsic
      )
  in
  Math.clamp_between (Math.clamp_non_negative value) min_opt max_opt

let main_axis_sizing = fun direction (style: Style.t) ->
  match direction with
  | Style.LeftToRight -> style.sizing.width
  | Style.TopToBottom -> style.sizing.height

let cross_axis_sizing = fun direction (style: Style.t) ->
  match direction with
  | Style.LeftToRight -> style.sizing.height
  | Style.TopToBottom -> style.sizing.width

let is_main_axis_grow = fun direction (style: Style.t) ->
  match main_axis_sizing direction style with
  | Style.Grow -> true
  | _ -> false

let align_h = fun align leftover ->
  match align with
  | Style.Left -> 0.0
  | Style.Center -> leftover /. 2.0
  | Style.Right -> leftover

let align_v = fun align leftover ->
  match align with
  | Style.Top -> 0.0
  | Style.Middle -> leftover /. 2.0
  | Style.Bottom -> leftover

let container_main_offset = fun direction (style: Style.t) leftover ->
  match direction with
  | Style.LeftToRight -> align_h style.alignment.x leftover
  | Style.TopToBottom -> align_v style.alignment.y leftover

let container_cross_offset = fun direction (style: Style.t) leftover ->
  match direction with
  | Style.LeftToRight -> align_v style.alignment.y leftover
  | Style.TopToBottom -> align_h style.alignment.x leftover

let text_align_offset = fun (style: Style.t) leftover ->
  match style.text_align with
  | Style.TextLeft -> 0.0
  | Style.TextCenter -> leftover /. 2.0
  | Style.TextRight -> leftover

let child_available_width = fun parent_inner_width (style: Style.t) ->
  Math.option_subtract
    parent_inner_width
    (Box_model.margin_horizontal style.margin)

let child_available_height = fun parent_inner_height (style: Style.t) ->
  Math.option_subtract
    parent_inner_height
    (Box_model.margin_vertical style.margin)

let content_width_constraint = fun (constraints: node_constraints) (style: Style.t) ->
  Math.option_subtract
    (
      match constraints.forced_width with
      | Some width -> Some width
      | None -> constraints.max_width
    )
    (Box_model.horizontal_inset style)

let content_height_constraint = fun (constraints: node_constraints) (style: Style.t) ->
  Math.option_subtract
    (
      match constraints.forced_height with
      | Some height -> Some height
      | None -> constraints.max_height
    )
    (Box_model.vertical_inset style)

let text_width_constraint = fun (constraints: node_constraints) (style: Style.t) ->
  let width_hint =
    match constraints.forced_width with
    | Some width -> Some width
    | None -> (
        match resolve_non_fit_size style.sizing.width constraints.max_width with
        | Some width -> Some width
        | None ->
            match style.text_wrap with
            | Style.NoWrap -> None
            | Style.Words
            | Style.Character -> constraints.max_width
      )
  in
  Math.option_subtract width_hint (Box_model.horizontal_inset style)

let rec measure_node: layout_node -> node_constraints -> Super.Config.t -> unit = fun
  node constraints config ->
  match node.element with
  | Element.Text { content; _ } -> measure_text_node node constraints config content
  | Element.Empty -> measure_intrinsic_node node constraints zero_viewport
  | Element.Custom { measure; _ } ->
      measure_intrinsic_node
        node
        constraints
        (measure
          ~constraints:(measurement_constraints
            ?available_width:(content_width_constraint constraints node.style)
            ?available_height:(content_height_constraint constraints node.style)
            ()))
  | Element.Container _ -> measure_container node constraints config

and measure_intrinsic_node: layout_node -> node_constraints -> Viewport.t -> unit = fun
  node constraints intrinsic ->
  let style = node.style in
  let width =
    resolve_axis_size
      style.sizing.width
      (intrinsic.width +. Box_model.horizontal_inset style)
      constraints.max_width
      constraints.forced_width
      style.sizing.min_width
      style.sizing.max_width
  in
  let height =
    resolve_axis_size
      style.sizing.height
      (intrinsic.height +. Box_model.vertical_inset style)
      constraints.max_height
      constraints.forced_height
      style.sizing.min_height
      style.sizing.max_height
  in
  node.computed_size <- Viewport.make ~width ~height

and measure_text_node: layout_node -> node_constraints -> Super.Config.t -> string -> unit = fun
  node constraints config content ->
  let style = node.style in
  let measurement =
    config.Super.Config.text_measurer
      ~constraints:(measurement_constraints
        ?available_width:(text_width_constraint constraints style)
        ?available_height:(content_height_constraint constraints style)
        ())
      content
      style
  in
  node.measured_text <- Some measurement;
  measure_intrinsic_node node constraints measurement.size

and measure_container: layout_node -> node_constraints -> Super.Config.t -> unit = fun
  node constraints config ->
  let style = node.style in
  let width_probe =
    match constraints.forced_width with
    | Some width -> Some width
    | None -> (
        match resolve_non_fit_size style.sizing.width constraints.max_width with
        | Some width -> Some width
        | None -> constraints.max_width
      )
  in
  let height_probe =
    match constraints.forced_height with
    | Some height -> Some height
    | None -> (
        match resolve_non_fit_size style.sizing.height constraints.max_height with
        | Some height -> Some height
        | None -> constraints.max_height
      )
  in
  measure_children_for_intrinsic_size
    node
    ~parent_inner_width:(Math.option_subtract width_probe (Box_model.horizontal_inset style))
    ~parent_inner_height:(Math.option_subtract height_probe (Box_model.vertical_inset style))
    config;
  let (intrinsic_width, intrinsic_height) = container_intrinsic_size node in
  let width =
    resolve_axis_size
      style.sizing.width
      (intrinsic_width +. Box_model.horizontal_inset style)
      constraints.max_width
      constraints.forced_width
      style.sizing.min_width
      style.sizing.max_width
  in
  let height =
    resolve_axis_size
      style.sizing.height
      (intrinsic_height +. Box_model.vertical_inset style)
      constraints.max_height
      constraints.forced_height
      style.sizing.min_height
      style.sizing.max_height
  in
  node.computed_size <- Viewport.make ~width ~height;
  measure_children_for_final_size
    node
    ~parent_inner_width:(Some (Math.clamp_non_negative (width -. Box_model.horizontal_inset style)))
    ~parent_inner_height:(Some (Math.clamp_non_negative (height -. Box_model.vertical_inset style)))
    config

and measure_children_for_intrinsic_size:
  layout_node ->
  parent_inner_width:float option ->
  parent_inner_height:float option ->
  Super.Config.t ->
  unit = fun node ~parent_inner_width ~parent_inner_height config ->
  let direction = node.style.direction in
  List.for_each
    node.children
    ~fn:(fun child ->
      let forced_main =
        if is_main_axis_grow direction child.style then
          Some 0.0
        else
          None
      in
      measure_child child direction ~parent_inner_width ~parent_inner_height ~forced_main config)

and measure_children_for_final_size:
  layout_node ->
  parent_inner_width:float option ->
  parent_inner_height:float option ->
  Super.Config.t ->
  unit = fun node ~parent_inner_width ~parent_inner_height config ->
  let direction = node.style.direction in
  let gap_count = Math.list_last_index node.children in
  let gap_space = Float.from_int (node.style.child_gap * gap_count) in
  List.for_each
    node.children
    ~fn:(fun child ->
      if not (is_main_axis_grow direction child.style) then
        measure_child
          child
          direction
          ~parent_inner_width
          ~parent_inner_height
          ~forced_main:None
          config);
  let fixed_outer_main =
    List.fold_left
      node.children
      ~init:0.0
      ~fn:(fun acc child ->
        if is_main_axis_grow direction child.style then
          acc
        else
          acc +. (
            match direction with
            | Style.LeftToRight -> Box_model.outer_width child
            | Style.TopToBottom -> Box_model.outer_height child
          ))
  in
  let grow_margin_main =
    List.fold_left
      node.children
      ~init:0.0
      ~fn:(fun acc child ->
        if is_main_axis_grow direction child.style then
          acc +. (
            match direction with
            | Style.LeftToRight -> Box_model.margin_horizontal child.style.margin
            | Style.TopToBottom -> Box_model.margin_vertical child.style.margin
          )
        else
          acc)
  in
  let total_weight =
    List.fold_left
      node.children
      ~init:0.0
      ~fn:(fun acc child ->
        if is_main_axis_grow direction child.style then
          acc +. child.style.grow_weight
        else
          acc)
  in
  let total_available =
    match direction with
    | Style.LeftToRight -> Math.option_default parent_inner_width 0.0
    | Style.TopToBottom -> Math.option_default parent_inner_height 0.0
  in
  let remaining =
    Math.clamp_non_negative (total_available -. gap_space -. fixed_outer_main -. grow_margin_main)
  in
  List.for_each
    node.children
    ~fn:(fun child ->
      if is_main_axis_grow direction child.style then
        let share =
          if (
            match Float.compare total_weight 0.0 with
            | Order.GT -> true
            | Order.LT
            | Order.EQ -> false
          ) then
            remaining *. (child.style.grow_weight /. total_weight)
          else
            0.0
        in
        measure_child
          child
          direction
          ~parent_inner_width
          ~parent_inner_height
          ~forced_main:(Some share)
          config)

and measure_child:
  layout_node ->
  Style.direction ->
  parent_inner_width:float option ->
  parent_inner_height:float option ->
  forced_main:float option ->
  Super.Config.t ->
  unit = fun child direction ~parent_inner_width ~parent_inner_height ~forced_main config ->
  let style = child.style in
  let available_width = child_available_width parent_inner_width style in
  let available_height = child_available_height parent_inner_height style in
  let default_width = resolve_non_fit_size style.sizing.width available_width in
  let default_height = resolve_non_fit_size style.sizing.height available_height in
  let forced_width =
    match direction with
    | Style.LeftToRight -> (
        match forced_main with
        | Some width -> Some width
        | None -> default_width
      )
    | Style.TopToBottom -> default_width
  in
  let forced_height =
    match direction with
    | Style.LeftToRight -> default_height
    | Style.TopToBottom -> (
        match forced_main with
        | Some height -> Some height
        | None -> default_height
      )
  in
  measure_node
    child
    {
      max_width = available_width;
      max_height = available_height;
      forced_width;
      forced_height;
    }
    config

and container_intrinsic_size: layout_node -> float * float = fun node ->
  let direction = node.style.direction in
  let (total_main, max_cross) =
    List.fold_left
      node.children
      ~init:(0.0, 0.0)
      ~fn:(fun (total_main, max_cross) child ->
        let (child_main, child_cross) =
          match direction with
          | Style.LeftToRight -> (Box_model.outer_width child, Box_model.outer_height child)
          | Style.TopToBottom -> (Box_model.outer_height child, Box_model.outer_width child)
        in
        (total_main +. child_main, Math.float_max max_cross child_cross))
  in
  let gap_space = Float.from_int (node.style.child_gap * Math.list_last_index node.children) in
  match direction with
  | Style.LeftToRight -> (total_main +. gap_space, max_cross)
  | Style.TopToBottom -> (max_cross, total_main +. gap_space)

let rec arrange_node: layout_node -> Geometry.Point.t -> unit = fun node origin ->
  node.final_box <- Geometry.Rect.make
    ~x:origin.x
    ~y:origin.y
    ~width:node.computed_size.width
    ~height:node.computed_size.height;
  match node.element with
  | Element.Container _ -> arrange_children node
  | Element.Text _
  | Element.Empty
  | Element.Custom _ -> ()

and arrange_children: layout_node -> unit = fun node ->
  let style = node.style in
  let rect = node.final_box in
  let content_rect = Box_model.content_box rect style in
  let (inner_main, inner_cross) =
    match style.direction with
    | Style.LeftToRight -> (content_rect.width, content_rect.height)
    | Style.TopToBottom -> (content_rect.height, content_rect.width)
  in
  let total_children_main =
    List.fold_left
      node.children
      ~init:0.0
      ~fn:(fun acc child ->
        acc +. (
          match style.direction with
          | Style.LeftToRight -> Box_model.outer_width child
          | Style.TopToBottom -> Box_model.outer_height child
        )) +. Float.from_int (style.child_gap * Math.list_last_index node.children)
  in
  let main_offset =
    container_main_offset
      style.direction
      style
      (Math.clamp_non_negative (inner_main -. total_children_main))
  in
  let cursor = ref main_offset in
  List.for_each
    node.children
    ~fn:(fun child ->
      let child_outer_cross =
        match style.direction with
        | Style.LeftToRight -> Box_model.outer_height child
        | Style.TopToBottom -> Box_model.outer_width child
      in
      let cross_offset =
        container_cross_offset
          style.direction
          style
          (Math.clamp_non_negative (inner_cross -. child_outer_cross))
      in
      let child_origin =
        match style.direction with
        | Style.LeftToRight ->
            Geometry.Point.make
              ~x:(content_rect.x +. !cursor +. Float.from_int child.style.margin.left)
              ~y:(content_rect.y +. cross_offset +. Float.from_int child.style.margin.top)
        | Style.TopToBottom ->
            Geometry.Point.make
              ~x:(content_rect.x +. cross_offset +. Float.from_int child.style.margin.left)
              ~y:(content_rect.y +. !cursor +. Float.from_int child.style.margin.top)
      in
      arrange_node child child_origin;
      cursor := !cursor +. (
        match style.direction with
        | Style.LeftToRight -> Box_model.outer_width child
        | Style.TopToBottom -> Box_model.outer_height child
      ) +. Float.from_int style.child_gap)

let rect_equal = fun (left: Geometry.Rect.t) (right: Geometry.Rect.t) ->
  Float.compare left.x right.x = Order.EQ
  && Float.compare left.y right.y = Order.EQ
  && Float.compare left.width right.width = Order.EQ
  && Float.compare left.height right.height = Order.EQ

let push_annotated = fun commands ~clip_stack command ->
  Vector.push
    commands
    ~value:{ command; clip_stack }

let clip_rect = fun (node: layout_node) ->
  let rect = Box_model.content_box node.final_box node.style in
  if (
    match Float.compare rect.width 0.0 with
    | Order.GT -> true
    | Order.LT
    | Order.EQ -> false
  ) && (
    match Float.compare rect.height 0.0 with
    | Order.GT -> true
    | Order.LT
    | Order.EQ -> false
  ) then
    Some rect
  else
    None

let child_clip_stack = fun (node: layout_node) clip_stack ->
  match (node.style.Style.overflow, clip_rect node) with
  | (Style.Clip, Some rect) -> clip_stack @ [ rect ]
  | _ -> clip_stack

let push_background = fun node commands ~clip_stack ->
  match node.style.background with
  | Some color when (
    match Float.compare node.final_box.width 0.0 with
    | Order.GT -> true
    | Order.LT
    | Order.EQ -> false
  ) && (
    match Float.compare node.final_box.height 0.0 with
    | Order.GT -> true
    | Order.LT
    | Order.EQ -> false
  ) ->
      push_annotated
        commands
        ~clip_stack
        {
          Render.bounding_box = node.final_box;
          command_type = Render.Rectangle { color; corner_radius = node.style.corner_radius };
          z_index = node.style.z_index;
        }
  | _ -> ()

let push_border = fun node commands ~clip_stack ->
  let width = Box_model.border_thickness node.style in
  if width > 0 then
    match node.style.border_color with
    | Some color ->
        push_annotated
          commands
          ~clip_stack
          {
            Render.bounding_box = node.final_box;
            command_type =
              Render.Border {
                width =
                  {
                    left = width;
                    right = width;
                    top = width;
                    bottom = width;
                  };
                color;
                corner_radius = node.style.corner_radius;
              };
            z_index = node.style.z_index;
          }
    | None -> ()

let push_text = fun node commands ~clip_stack content ->
  let style = node.style in
  let text_rect = Box_model.content_box node.final_box style in
  if (
    match Float.compare text_rect.width 0.0 with
    | Order.LT
    | Order.EQ -> true
    | Order.GT -> false
  ) || (
    match Float.compare text_rect.height 0.0 with
    | Order.LT
    | Order.EQ -> true
    | Order.GT -> false
  ) then
    ()
  else
    let measurement =
      match node.measured_text with
      | Some measurement -> measurement
      | None ->
          Super.Config.default_text_measurer
            ~constraints:(measurement_constraints
              ~available_width:text_rect.width
              ~available_height:text_rect.height
              ())
            content
            style
    in
    let max_lines = Int.max 0 (Float.to_int (Float.floor text_rect.height)) in
    let color = Math.option_default style.foreground (`rgb (255, 255, 255)) in
    let rec loop line_index = fun __tmp1 ->
      match __tmp1 with
      | [] -> ()
      | _ when line_index >= max_lines -> ()
      | line :: rest ->
          let line_width = Float.from_int (String.width line) in
          let visible_width = Math.float_min line_width text_rect.width in
          let line_x =
            text_rect.x
            +. text_align_offset style (Math.clamp_non_negative (text_rect.width -. line_width))
          in
          if line != "" then
            push_annotated
              commands
              ~clip_stack
              {
                Render.bounding_box = Geometry.Rect.make
                  ~x:line_x
                  ~y:(text_rect.y +. Float.from_int line_index)
                  ~width:visible_width
                  ~height:1.0;
                command_type =
                  Render.Text {
                    content = line;
                    color;
                    size = style.text_size;
                    weight = style.font_weight;
                    decoration = style.text_decoration;
                  };
                z_index = style.z_index;
              };
          loop (line_index + 1) rest
    in
    loop 0 measurement.lines

let rec generate_commands: layout_node -> clipped_command Vector.t -> Geometry.Rect.t list -> unit = fun
  node commands clip_stack ->
  let child_clip_stack = child_clip_stack node clip_stack in
  push_background node commands ~clip_stack;
  push_border node commands ~clip_stack;
  (
    match node.element with
    | Element.Text { content; _ } -> push_text node commands ~clip_stack:child_clip_stack content
    | Element.Container _ ->
        List.for_each
          node.children
          ~fn:(fun child ->
            generate_commands child commands child_clip_stack)
    | Element.Custom { render; _ } ->
        let custom_commands = render node.final_box in
        List.for_each
          custom_commands
          ~fn:(fun command ->
            push_annotated commands ~clip_stack:child_clip_stack command)
    | Element.Empty -> ()
  )

let index_commands = fun commands ->
  let rec loop index acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.rev acc
    | command :: rest -> loop (index + 1) ((index, command) :: acc) rest
  in
  loop 0 [] commands

let rec common_prefix_length = fun left right ->
  match (left, right) with
  | (left_head :: left_tail, right_head :: right_tail) when rect_equal left_head right_head ->
      1 + common_prefix_length left_tail right_tail
  | _ -> 0

let rec drop = fun count values ->
  if count <= 0 then
    values
  else
    match values with
    | [] -> []
    | _ :: rest -> drop (count - 1) rest

let rec emit_scissor_ends = fun output z_index ->
  fun __tmp1 ->
    match __tmp1 with
    | [] -> ()
    | _ :: rest ->
        emit_scissor_ends output z_index rest;
        Vector.push
          output
          ~value:{
            Render.bounding_box = Geometry.Rect.zero;
            command_type = Render.ScissorEnd;
            z_index;
          }

let emit_scissor_starts = fun output z_index rects ->
  List.for_each
    rects
    ~fn:(fun rect ->
      Vector.push
        output
        ~value:{ Render.bounding_box = rect; command_type = Render.ScissorStart rect; z_index })

let emit_scissor_transition = fun output ~current ~target ~z_index ->
  let common = common_prefix_length current target in
  emit_scissor_ends
    output
    z_index
    (drop common current);
  emit_scissor_starts
    output
    z_index
    (drop common target)

let linearize_commands = fun commands ->
  let output = Vector.create () in
  let rec loop current_clip_stack last_z = fun __tmp1 ->
    match __tmp1 with
    | [] -> emit_scissor_transition output ~current:current_clip_stack ~target:[] ~z_index:last_z
    | (_, annotated) :: rest ->
        let z_index = annotated.command.Render.z_index in
        emit_scissor_transition
          output
          ~current:current_clip_stack
          ~target:annotated.clip_stack
          ~z_index;
        Vector.push output ~value:annotated.command;
        loop annotated.clip_stack z_index rest
  in
  loop [] 0 commands;
  output
  |> Vector.iter
  |> Iter.Iterator.to_list

let compute = fun ~config element ->
  let layout_tree = build_layout_tree element in
  measure_node
    layout_tree
    {
      empty_node_constraints with
      max_width = Some config.Super.Config.viewport.width;
      max_height = Some config.Super.Config.viewport.height;
    }
    config;
  arrange_node layout_tree Geometry.Point.zero;
  let commands = Vector.create () in
  generate_commands layout_tree commands [];
  let command_list =
    commands
    |> Vector.iter
    |> Iter.Iterator.to_list
  in
  index_commands command_list
  |> List.sort
    ~compare:(fun (left_index, left) (right_index, right) ->
      let by_z = Int.compare left.command.Render.z_index right.command.Render.z_index in
      match by_z with
      | Order.LT
      | Order.GT -> by_z
      | Order.EQ -> Int.compare left_index right_index)
  |> linearize_commands
