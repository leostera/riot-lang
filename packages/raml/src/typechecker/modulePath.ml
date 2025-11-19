open Std

type t = Identifier of Identifier.t | Dot of t * string | Apply of t * t

let rec same p1 p2 =
  Ptr.equal p1 p2
  ||
  match (p1, p2) with
  | Identifier id1, Identifier id2 -> Identifier.same id1 id2
  | Dot (p1, s1), Dot (p2, s2) -> s1 = s2 && same p1 p2
  | Apply (f1, a1), Apply (f2, a2) -> same f1 f2 && same a1 a2
  | _ -> false

let rec compare p1 p2 =
  if Ptr.equal p1 p2 then 0
  else
    match (p1, p2) with
    | Identifier id1, Identifier id2 -> Identifier.compare id1 id2
    | Dot (p1, s1), Dot (p2, s2) ->
        let h = compare p1 p2 in
        if h != 0 then h else String.compare s1 s2
    | Apply (f1, a1), Apply (f2, a2) ->
        let h = compare f1 f2 in
        if h != 0 then h else compare a1 a2
    | Identifier _, (Dot _ | Apply _) -> -1
    | Dot _, Apply _ -> -1
    | (Dot _ | Apply _), Identifier _ -> 1
    | Apply _, Dot _ -> 1

let rec scope = function
  | Identifier id -> Identifier.scope id
  | Dot (p, _) -> scope p
  | Apply (p1, p2) -> Int.max (scope p1) (scope p2)

let rec name ?(paren = fun _ -> false) = function
  | Identifier id -> Identifier.name id
  | Dot (p, s) ->
      let s_escaped = s in
      name ~paren p
      ^ if paren s then ".( " ^ s_escaped ^ " )" else "." ^ s_escaped
  | Apply (p1, p2) -> name ~paren p1 ^ "(" ^ name ~paren p2 ^ ")"

let rec head = function
  | Identifier id -> id
  | Dot (p, _) | Apply (p, _) -> head p

let to_string p = name p
