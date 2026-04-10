(* Basic multicore domain spawn/join. *)
let child =
  Domain.spawn (fun () ->
      List.fold_left ( + ) 0 [ 1; 2; 3; 4; 5 ])

let () =
  let result = Domain.join child in
  Printf.printf "%d\n" result
