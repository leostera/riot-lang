(* Lazy suspension and memoization. *)
let counter = ref 0

let thunk =
  lazy
    (incr counter;
     21 + 21)

let () =
  let before = !counter in
  let value1 = Lazy.force thunk in
  let after1 = !counter in
  Printf.printf "%d %d %d\n" before value1 after1;
  let value2 = Lazy.force thunk in
  let after2 = !counter in
  Printf.printf "%d %d\n" value2 after2
