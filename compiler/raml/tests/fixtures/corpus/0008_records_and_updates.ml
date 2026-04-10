(* Immutable records and functional update. *)
type point = { x : int; y : int }

let move dx dy p = { p with x = p.x + dx; y = p.y + dy }

let () =
  let p = { x = 1; y = 2 } in
  let q = move 3 4 p in
  Printf.printf "(%d,%d) -> (%d,%d)\n" p.x p.y q.x q.y
