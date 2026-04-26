open Std

type t =
  | Text of {
      style: Style.t;
      content: string;
    }
  | Container of {
      style: Style.t;
      children: t list;
    }
  | Empty
  | Custom of {
      style: Style.t;
      measure: (constraints:Super.Config.constraints -> Viewport.t);
      render: Geometry.Rect.t -> Render.command list;
    }

let text = fun ?(style = Style.empty) content -> Text { style; content }

let container = fun ?(style = Style.empty) children -> Container { style; children }

let empty = Empty

let custom = fun ?(style = Style.empty) ~measure ~render () -> Custom { style; measure; render }

let row = fun ?(style = Style.empty) children ->
  let style =
    Style.(style
    |> row)
  in
  Container { style; children }

let column = fun ?(style = Style.empty) children ->
  let style =
    Style.(style
    |> column)
  in
  Container { style; children }

let spacer = fun ?(flex = 1.0) () ->
  Container {
    style =
      Style.(empty
      |> grow
      |> grow_weight flex);
    children = [];
  }
