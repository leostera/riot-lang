(* Simple local module definition *)

let result =
  let module M = struct
    let x = 42
  end in
  M.x
