open Std
open Gooey

let make_config = fun ?(width = 120.0) ?(height = 40.0) () ->
  Config.make ~viewport:(Viewport.make ~width ~height) ~text_measurer:Config.default_text_measurer ()

let badge = fun label ->
  Element.text
    ~style:Style.(empty |> padding (Style.Padding.symmetric ~h:1 ~v:0) |> border ~width:1 ())
    label

let make_wide_row = fun count ->
  let children =
    List.init ~count ~fn:(fun index -> badge ("item-" ^ Int.to_string index))
  in
  Element.row
    ~style:Style.(empty
    |> width (Fixed 320.0)
    |> child_gap 1
    |> padding (Style.Padding.symmetric ~h:1 ~v:1))
    children

let rec make_deep_tree = fun depth leaf ->
  if depth <= 0 then
    leaf
  else
    Element.column
      ~style:Style.(empty |> width Fit |> height Fit |> padding (Style.Padding.symmetric ~h:1 ~v:0))
      [ make_deep_tree (depth - 1) leaf; ]

let make_sidebar = fun item_count ->
  let items =
    List.init
      ~count:item_count
      ~fn:(fun index ->
        Element.text
          ~style:Style.(empty |> width Grow |> padding (Style.Padding.symmetric ~h:1 ~v:0))
          ("section-" ^ Int.to_string index))
  in
  Element.column
    ~style:Style.(empty
    |> width (Fixed 24.0)
    |> height Grow
    |> padding (Style.Padding.all 1)
    |> child_gap 1
    |> bg (Style.color "#16202A"))
    items

let make_card = fun title body ->
  Element.column
    ~style:Style.(empty
    |> width Grow
    |> padding (Style.Padding.all 1)
    |> child_gap 1
    |> border ~width:1 ~color:(Style.color "#5A6B7A") ()
    |> bg (Style.color "#0F1720")
    |> clip)
    [
      Element.text ~style:Style.(empty |> bold |> fg (Style.color "#F8FAFC")) title;
      Element.text
        ~style:Style.(empty |> width Grow |> text_wrap Words |> fg (Style.color "#CBD5E1"))
        body;
    ]

let make_mixed_dashboard = fun () ->
  let body = "Unicode-heavy text: Hello 世界 👍🏽 cafe\u{301} metrics latency throughput clipping wrapping borders alignment" in
  let cards =
    List.init ~count:10 ~fn:(fun index -> make_card ("card-" ^ Int.to_string index) body)
  in
  let content = Element.column
    ~style:Style.(empty
    |> width Grow
    |> height Grow
    |> padding (Style.Padding.all 1)
    |> child_gap 1
    |> bg (Style.color "#111827"))
    cards in
  let inspector = Element.column
    ~style:Style.(empty
    |> width (Percent 0.2)
    |> height Grow
    |> padding (Style.Padding.all 1)
    |> child_gap 1
    |> bg (Style.color "#1F2937"))
    [ badge "filters"; badge "selection"; badge "events"; badge "search"; badge "help"; ] in
  Element.row
    ~style:Style.(empty
    |> width (Fixed 120.0)
    |> height (Fixed 36.0)
    |> padding (Style.Padding.all 1)
    |> child_gap 1
    |> bg (Style.color "#020617"))
    [ make_sidebar 12; content; inspector; ]

let make_wrapped_unicode_document = fun count ->
  let lines =
    List.init
      ~count
      ~fn:(fun index ->
        Element.text
          ~style:Style.(empty
          |> width Grow
          |> text_wrap Words
          |> padding (Style.Padding.symmetric ~h:1 ~v:0))
          ("line " ^ Int.to_string index ^ ": Hello 世界 👍🏽 cafe\u{301} -- wrapped unicode content for Gooey benchmarks"))
  in
  Element.column
    ~style:Style.(empty |> width (Fixed 64.0) |> padding (Style.Padding.all 1) |> child_gap 1 |> clip)
    lines

let render_custom_bar = fun label width box ->
  let data = String.make ~len:width ~char:'#' ^ " " ^ label in
  [ { Render.bounding_box = box; command_type = Render.Custom { data }; z_index = 2 }; ]

let make_custom_widget_board = fun count ->
  let widget index =
    Element.custom ~style:Style.(empty
    |> width Grow
    |> height (Fixed 1.0)
    |> padding (Style.Padding.symmetric ~h:1 ~v:0))
      ~measure:(fun ~constraints ->
        let width =
          match constraints.Config.available_width with
          | Some width ->
              if Float.compare width 24.0 = Order.LT then
                width
              else
                24.0
          | None -> 24.0
        in
        Viewport.make ~width ~height:1.0)
      ~render:(fun box -> render_custom_bar ("w" ^ Int.to_string index) 8 box)
      ()
  in
  Element.column
    ~style:Style.(empty |> width (Fixed 40.0) |> padding (Style.Padding.all 1) |> child_gap 1 |> clip)
    (List.init ~count ~fn:widget)

let bench_layout_wide_row = fun () ->
  let commands = layout ~config:(make_config ~width:320.0 ~height:24.0 ()) (make_wide_row 300) in
  ignore (List.length commands)

let bench_layout_deep_tree = fun () ->
  let tree = make_deep_tree 60 (Element.text "leaf") in
  let commands = layout ~config:(make_config ~width:96.0 ~height:240.0 ()) tree in
  ignore (List.length commands)

let bench_layout_mixed_dashboard = fun () ->
  let commands = layout ~config:(make_config ~width:120.0 ~height:40.0 ()) (make_mixed_dashboard ()) in
  ignore (List.length commands)

let bench_layout_wrapped_unicode = fun () ->
  let commands = layout
    ~config:(make_config ~width:80.0 ~height:120.0 ())
    (make_wrapped_unicode_document 80) in
  ignore (List.length commands)

let bench_layout_custom_widgets = fun () ->
  let commands = layout
    ~config:(make_config ~width:48.0 ~height:80.0 ())
    (make_custom_widget_board 40) in
  ignore (List.length commands)

let dashboard_commands = layout
  ~config:(make_config ~width:120.0 ~height:40.0 ())
  (make_mixed_dashboard ())

let bench_inline_renderer = fun () ->
  let output = Terminal_renderer_inline.render_to_string dashboard_commands in
  ignore (String.length output)

let bench_fullscreen_renderer = fun () ->
  let output = Terminal_renderer_fullscreen.render_to_string dashboard_commands in
  ignore (String.length output)

let medium: Bench.bench_config = { iterations = 3; warmup = 1 }

let heavy: Bench.bench_config = { iterations = 2; warmup = 1 }

let benchmarks =
  Bench.[
    with_config ~config:medium "gooey layout wide row" bench_layout_wide_row;
    skip "gooey layout deep tree" bench_layout_deep_tree;
    with_config ~config:medium "gooey layout mixed dashboard" bench_layout_mixed_dashboard;
    with_config ~config:medium "gooey layout wrapped unicode" bench_layout_wrapped_unicode;
    with_config ~config:medium "gooey layout custom widgets" bench_layout_custom_widgets;
    with_config ~config:heavy "gooey renderer inline dashboard" bench_inline_renderer;
    with_config ~config:heavy "gooey renderer fullscreen dashboard" bench_fullscreen_renderer;
  ]

let main ~args = Bench.Cli.main ~name:"gooey benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
