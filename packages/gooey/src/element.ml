open Std

type t =
  | Text of { style: Style.t; content: string; }
  | Container of { style: Style.t; children: t list; }
  | Empty
  | Custom of {
      style: Style.t;
      measure: unit -> Viewport.t;
      render: Geometry.Rect.t -> Render.command list;
    }

let text = fun ?(style = Style.empty) content -> Text {style; content}

let container = fun ?(style = Style.empty) children -> Container {style; children}

let empty = Empty

let custom = fun ?(style = Style.empty) ~measure ~render () -> Custom {style; measure; render}

let row = fun ?(style = Style.empty) children -> Container {style = Style.row style; children}

let column = fun ?(style = Style.empty) children -> Container {style = Style.column style; children}

let spacer = fun ?(flex = 1.0) () ->
    Container {style = Style.(empty |> size ~width:(Fixed flex) ~height:Grow); children = []}
