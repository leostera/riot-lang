module rec A: sig
  val x: int
end = struct
  let x = B.y
end

and B: sig
  val y: int
end = struct
  let y = 1
end
