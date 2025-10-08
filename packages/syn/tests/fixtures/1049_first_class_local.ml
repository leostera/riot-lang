let () =
  let module M = struct
    let x = 42
  end in
  print_int M.x
