open Std

(** Position specification for layers *)
type position =
  | Relative  (* Normal flow positioning *)
  | Absolute of int * int  (* Absolute positioning at (x, y) *)

(** Core element type - recursive tree structure *)
type t =
  | Text of Style.t * string
  | Box of Style.t * t
  | Row of Style.t * t list
  | Column of Style.t * t list
  | Layer of Style.t * position * t list  (* Stack elements with positioning *)
  | Spacer of Style.t
  | Empty

(** Structural equality for elements *)
let rec equal a b =
  match (a, b) with
  | Empty , Empty -> true
  | Text (s1, t1), Text (s2, t2) -> Style.equal s1 s2 && String.equal t1 t2
  | Box (s1, c1), Box (s2, c2) -> Style.equal s1 s2 && equal c1 c2
  | Spacer s1, Spacer s2 -> Style.equal s1 s2
  | Row (s1, cs1), Row (s2, cs2) -> Style.equal s1 s2 && List.length cs1 = List.length cs2 && List.for_all2 equal cs1 cs2
  | Column (s1, cs1), Column (s2, cs2) -> Style.equal s1 s2 && List.length cs1 = List.length cs2 && List.for_all2 equal cs1 cs2
  | Layer (s1, p1, cs1), Layer (s2, p2, cs2) -> Style.equal s1 s2 && p1 = p2 && List.length cs1 = List.length cs2 && List.for_all2 equal cs1 cs2
  | _ -> false

(** ## Basic Elements *)

let empty = Empty

let text ?(style = Style.default) str = Text (style, str)

let box ?(style = Style.default) child = Box (style, child)

(** ## Container Elements *)

let row ?(style = Style.default) children = Row (style, children)

let column ?(style = Style.default) children = Column (style, children)

let layer ?(style = Style.default) ?(pos = Relative) children = Layer (style, pos, children)

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
