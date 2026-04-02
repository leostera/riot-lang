module M: sig
  include S

  val extra: int
end = struct
  include N

  let extra = 42
end
