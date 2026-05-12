module F (X : S): sig
  val x: int
end = struct
  let x = X.value
end
