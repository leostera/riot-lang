let x =
  match y with
  | [] -> 0
  | [ a ] -> a
  | a :: b :: _ -> a + b
