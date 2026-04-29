type mode =
  | Local
  | Generalized

type t = {
  quantifier: string list;
  body: Ast.Type.t;
}

let monomorphic body = { quantifier = []; body }
