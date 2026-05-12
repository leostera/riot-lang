let make_pair n =
  (n, n + 1)

let () = Callback.register "zort_e2e_alloc_pair" make_pair
