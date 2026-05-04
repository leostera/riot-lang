open Std
open Suri
open Suri.Component

(** MyDesign - Example Reusable Component Library *)
module MyDesign = struct
  (* Design Tokens *)

  let primary_color = "#007bff"

  let secondary_color = "#6c757d"

  let success_color = "#28a745"

  let danger_color = "#dc3545"

  let warning_color = "#ffc107"

  let info_color = "#17a2b8"

  let spacing_xs = "4px"

  let spacing_sm = "8px"

  let spacing_md = "16px"

  let spacing_lg = "24px"

  let spacing_xl = "32px"

  let radius_sm = "4px"

  let radius_md = "8px"

  let radius_lg = "12px"

  (* Layout Components *)

  let container = fun ?(max_width = "1200px") children ->
    div
      ~attrs:[
        class_ "container";
        style_ ("max-width: " ^ max_width ^ "; margin: 0 auto; padding: 0 " ^ spacing_md);
      ]
      children

  let grid = fun ?(columns = 3) ?(gap = spacing_md) children ->
    div
      ~attrs:[
        class_ "grid";
        style_
          ("display: grid; grid-template-columns: repeat("
          ^ Int.to_string columns
          ^ ", 1fr); gap: "
          ^ gap
          ^ ";");
      ]
      children

  let stack = fun ?(spacing = spacing_md) children ->
    div
      ~attrs:[
        class_ "stack";
        style_ ("display: flex; flex-direction: column; gap: " ^ spacing ^ ";");
      ]
      children

  let row = fun ?(spacing = spacing_md) ?(align = "start") children ->
    div
      ~attrs:[
        class_ "row";
        style_ ("display: flex; gap: " ^ spacing ^ "; align-items: " ^ align ^ ";");
      ]
      children

  (* Typography *)

  let page_header = fun title subtitle ->
    header
      ~attrs:[ class_ "page-header" ]
      [
        h1 ~attrs:[ style_ ("margin: 0 0 " ^ spacing_sm ^ " 0") ] [ text title ];
        when_
          (subtitle != "")
          (p
            ~attrs:[ class_ "subtitle"; style_ "color: #666; margin: 0; font-size: 1.2em" ]
            [ text subtitle ]);
      ]

  (* Components *)

  let card = fun ?(class_extra = "") ?(style_extra = "") children ->
    div
      ~attrs:[
        class_ ("card " ^ class_extra);
        style_
          ("border: 1px solid #e0e0e0; border-radius: "
          ^ radius_md
          ^ "; padding: "
          ^ spacing_lg
          ^ "; background: white; "
          ^ "box-shadow: 0 2px 4px rgba(0,0,0,0.1); "
          ^ style_extra);
      ]
      children

  let button_base = fun
    ?(class_extra = "") ?(bg_color = primary_color) ?(text_color = "white") children ->
    button
      ~attrs:[
        class_ ("btn " ^ class_extra);
        style_
          ("background: "
          ^ bg_color
          ^ "; color: "
          ^ text_color
          ^ "; border: none; "
          ^ "padding: 10px 20px; border-radius: "
          ^ radius_sm
          ^ "; cursor: pointer; "
          ^ "font-weight: 600; transition: opacity 0.2s;");
      ]
      children

  let button_primary = fun ?(class_extra = "") children ->
    button_base
      ~class_extra:("btn-primary " ^ class_extra)
      ~bg_color:primary_color
      children

  let button_secondary = fun ?(class_extra = "") children ->
    button_base
      ~class_extra:("btn-secondary " ^ class_extra)
      ~bg_color:secondary_color
      children

  let button_success = fun ?(class_extra = "") children ->
    button_base
      ~class_extra:("btn-success " ^ class_extra)
      ~bg_color:success_color
      children

  let button_danger = fun ?(class_extra = "") children ->
    button_base
      ~class_extra:("btn-danger " ^ class_extra)
      ~bg_color:danger_color
      children

  let badge = fun ?(variant = "primary") content ->
    let bg_color =
      match variant with
      | "success" -> success_color
      | "danger" -> danger_color
      | "warning" -> warning_color
      | "info" -> info_color
      | "secondary" -> secondary_color
      | _ -> primary_color
    in
    let text_color =
      if variant = "warning" then
        "#000"
      else
        "#fff"
    in
    span
      ~attrs:[
        class_ ("badge badge-" ^ variant);
        style_
          ("background: "
          ^ bg_color
          ^ "; color: "
          ^ text_color
          ^ "; padding: 4px 10px; border-radius: "
          ^ radius_lg
          ^ "; font-size: 12px; font-weight: 600; display: inline-block;");
      ]
      [ text content ]

  let alert = fun ?(type_ = "info") ?(dismissible = false) children ->
    let (bg_color, border_color, text_color) =
      match type_ with
      | "success" -> ("#d4edda", "#c3e6cb", "#155724")
      | "danger" -> ("#f8d7da", "#f5c6cb", "#721c24")
      | "warning" -> ("#fff3cd", "#ffeaa7", "#856404")
      | _ -> ("#d1ecf1", "#bee5eb", "#0c5460")
    in
    div
      ~attrs:[
        class_ ("alert alert-" ^ type_);
        style_
          ("background: "
          ^ bg_color
          ^ "; border: 1px solid "
          ^ border_color
          ^ "; color: "
          ^ text_color
          ^ "; padding: "
          ^ spacing_md
          ^ "; border-radius: "
          ^ radius_sm
          ^ "; margin: "
          ^ spacing_sm
          ^ " 0; "
          ^ "display: flex; justify-content: space-between; align-items: center;");
      ]
      [
        div children;
        when_
          dismissible
          (button
            ~attrs:[
              class_ "close";
              style_
                "background: none; border: none; font-size: 24px; cursor: pointer; opacity: 0.5;";
              attr "aria-label" "Close";
            ]
            [ text "×" ]);
      ]

  let progress = fun ?(value = 0) ?(max = 100) () ->
    let percentage = (Float.from_int value /. Float.from_int max) *. 100.0 in
    div
      ~attrs:[
        class_ "progress";
        style_
          ("background: #e0e0e0; border-radius: " ^ radius_lg ^ "; height: 20px; overflow: hidden;");
      ]
      [
        div
          ~attrs:[
            class_ "progress-bar";
            style_
              ("background: "
              ^ primary_color
              ^ "; height: 100%; width: "
              ^ Float.to_string percentage
              ^ "%; transition: width 0.3s;");
            attr "role" "progressbar";
            attr "aria-valuenow" (Int.to_string value);
            attr "aria-valuemin" "0";
            attr "aria-valuemax" (Int.to_string max);
          ]
          [];
      ]
end

(** Example: Product Card Component *)
let product_card = fun ~name ~price ~in_stock ~discount ->
  MyDesign.card
    ~class_extra:"product-card"
    [
      MyDesign.stack
        ~spacing:MyDesign.spacing_md
        [
          div
            [
              h3 ~attrs:[ style_ "margin: 0 0 8px 0" ] [ text name ];
              MyDesign.row
                ~spacing:MyDesign.spacing_sm
                ~align:"center"
                [
                  MyDesign.badge
                    ~variant:(
                      if in_stock then
                        "success"
                      else
                        "danger"
                    )
                    (
                      if in_stock then
                        "In Stock"
                      else
                        "Out of Stock"
                    );
                  when_
                    (discount > 0)
                    (MyDesign.badge ~variant:"warning" ((Int.to_string discount) ^ "% OFF"));
                ];
            ];
          div
            ~attrs:[ style_ "font-size: 24px; font-weight: bold; color: #007bff" ]
            [ text "$"; text (Float.to_string price) ];
          MyDesign.button_primary [ text "Add to Cart" ];
        ];
    ]

(** Example Page *)
let example_page: unit t =
  html
    [
      head
        [
          title [ text "Design System Example" ];
          meta ~attrs:[ attr "charset" "UTF-8" ] ();
          meta ~attrs:[ attr "viewport" "width=device-width, initial-scale=1.0" ] ();
          style
            {|
        * { box-sizing: border-box; }
        body { 
          font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
          margin: 0;
          padding: 40px 0;
          background: #f5f5f5;
          color: #333;
        }
      |};
        ];
      body
        [
          MyDesign.container
            [
              MyDesign.stack
                ~spacing:MyDesign.spacing_xl
                [
                  MyDesign.page_header
                    "Design System Example"
                    "Reusable components for building beautiful UIs";
                  section
                    [
                      h2 [ text "Alerts" ];
                      MyDesign.stack
                        ~spacing:MyDesign.spacing_md
                        [
                          MyDesign.alert
                            ~type_:"info"
                            [ strong [ text "Info: " ]; text "This is an informational message." ];
                          MyDesign.alert
                            ~type_:"success"
                            ~dismissible:true
                            [ strong [ text "Success! " ]; text "Your changes have been saved." ];
                          MyDesign.alert
                            ~type_:"warning"
                            [ strong [ text "Warning: " ]; text "Please review your settings." ];
                          MyDesign.alert
                            ~type_:"danger"
                            [ strong [ text "Error: " ]; text "Something went wrong." ];
                        ];
                    ];
                  section
                    [
                      h2 [ text "Buttons" ];
                      MyDesign.row
                        ~spacing:MyDesign.spacing_md
                        [
                          MyDesign.button_primary [ text "Primary" ];
                          MyDesign.button_secondary [ text "Secondary" ];
                          MyDesign.button_success [ text "Success" ];
                          MyDesign.button_danger [ text "Danger" ];
                        ];
                    ];
                  section
                    [
                      h2 [ text "Badges" ];
                      MyDesign.row
                        ~spacing:MyDesign.spacing_md
                        [
                          MyDesign.badge ~variant:"primary" "Primary";
                          MyDesign.badge ~variant:"secondary" "Secondary";
                          MyDesign.badge ~variant:"success" "Success";
                          MyDesign.badge ~variant:"danger" "Danger";
                          MyDesign.badge ~variant:"warning" "Warning";
                          MyDesign.badge ~variant:"info" "Info";
                        ];
                    ];
                  section
                    [
                      h2 [ text "Progress Bars" ];
                      MyDesign.stack
                        ~spacing:MyDesign.spacing_md
                        [
                          div [ p [ text "25% Complete" ]; MyDesign.progress ~value:25 () ];
                          div [ p [ text "75% Complete" ]; MyDesign.progress ~value:75 () ];
                          div [ p [ text "100% Complete" ]; MyDesign.progress ~value:100 () ];
                        ];
                    ];
                  section
                    [
                      h2 [ text "Product Grid" ];
                      MyDesign.grid
                        ~columns:3
                        ~gap:MyDesign.spacing_lg
                        [
                          product_card ~name:"Widget Pro" ~price:29.99 ~in_stock:true ~discount:10;
                          product_card ~name:"Gadget Ultra" ~price:49.99 ~in_stock:true ~discount:0;
                          product_card
                            ~name:"Doohickey Max"
                            ~price:19.99
                            ~in_stock:false
                            ~discount:25;
                          product_card
                            ~name:"Thingamajig Plus"
                            ~price:39.99
                            ~in_stock:true
                            ~discount:15;
                          product_card
                            ~name:"Whatchamacallit"
                            ~price:24.99
                            ~in_stock:true
                            ~discount:0;
                          product_card
                            ~name:"Gizmo Supreme"
                            ~price:59.99
                            ~in_stock:false
                            ~discount:20;
                        ];
                    ];
                  footer
                    ~attrs:[
                      style_
                        ("margin-top: "
                        ^ MyDesign.spacing_xl
                        ^ "; padding-top: "
                        ^ MyDesign.spacing_lg
                        ^ "; border-top: 1px solid #ddd; text-align: center;");
                    ]
                    [
                      p
                        [
                          text "Built with ";
                          strong [ text "Suri.Component" ];
                          text " design system pattern";
                        ];
                    ];
                ];
            ];
        ];
    ]

let main ~args:_ =
  let html = to_html example_page in
  println html;
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
