let run x =
  let result = x + 2 in
  if result = 42 then result else -1

let () = Callback.register "zort_e2e_noalloc" run
