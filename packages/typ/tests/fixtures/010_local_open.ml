let clamp = fun mix ->
  Float.(min (max 0. mix) 1.)
