open Std

type pattern = Exact of Uri.t | Any | Variable of string

type query_result = {
  entity : Uri.t;
  attribute : Uri.t;
  value : Fact.value;
  bindings : (string * Fact.value) list;
}

let value_equal v1 v2 =
  match (v1, v2) with
  | Fact.String s1, Fact.String s2 -> String.equal s1 s2
  | Fact.Int i1, Fact.Int i2 -> Int.equal i1 i2
  | Fact.Bool b1, Fact.Bool b2 -> Bool.equal b1 b2
  | Fact.Float f1, Fact.Float f2 -> Float.equal f1 f2
  | Fact.Uri u1, Fact.Uri u2 -> Uri.equal u1 u2
  | Fact.DateTime d1, Fact.DateTime d2 -> d1 = d2
  | _ -> false
