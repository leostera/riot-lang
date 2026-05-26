let x =
  List.map (fun y -> y * 2) (List.filter (fun z -> z > 0) xs)
