open Std

(** Core element type - recursive tree structure *)
type t =
  | Text of Style.t * string
  | Box of Style.t * t
  | Row of Style.t * t list
  | Column of Style.t * t list
  | Spacer of Style.t
  | Empty of Style.t

(** ## Basic Elements *)

let text ?(style = Style.default) str = Text (style, str)

let box ?(style = Style.default) child = Box (style, child)

let empty ?(style = Style.default) () = Empty style

(** ## Container Elements *)

let row ?(style = Style.default) children = Row (style, children)

let column ?(style = Style.default) children = Column (style, children)

(** ## Spacing Elements *)

let spacer ?(style = Style.default) () = Spacer style

let h_space width =
  spacer ~style:(Style.default |> Style.width_fixed width) ()

let v_space height =
  spacer ~style:(Style.default |> Style.height_fixed height) ()

let h_flex ?(weight = 1.0) () =
  spacer ~style:(Style.default |> Style.width_flex weight) ()

let v_flex ?(weight = 1.0) () =
  spacer ~style:(Style.default |> Style.height_flex weight) ()

(** ## Convenience Functions *)

let spaced_row ?(gap = 1) ?(style = Style.default) children =
  let rec intersperse sep = function
    | [] -> []
    | [x] -> [x]
    | x :: xs -> x :: sep :: intersperse sep xs
  in
  let children_with_gaps = intersperse (h_space gap) children in
  Row (style, children_with_gaps)

let spaced_column ?(gap = 1) ?(style = Style.default) children =
  let rec intersperse sep = function
    | [] -> []
    | [x] -> [x]
    | x :: xs -> x :: sep :: intersperse sep xs
  in
  let children_with_gaps = intersperse (v_space gap) children in
  Column (style, children_with_gaps)
