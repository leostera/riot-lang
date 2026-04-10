(* Nested arrays and matrix multiplication. *)
let mul a b =
  let rows = Array.length a in
  let cols = Array.length b.(0) in
  let inner = Array.length b in
  Array.init rows (fun i ->
      Array.init cols (fun j ->
          let acc = ref 0 in
          for k = 0 to inner - 1 do
            acc := !acc + (a.(i).(k) * b.(k).(j))
          done;
          !acc))

let () =
  let a = [| [| 1; 2 |]; [| 3; 4 |] |] in
  let b = [| [| 5; 6 |]; [| 7; 8 |] |] in
  let c = mul a b in
  Array.iter
    (fun row ->
      Array.iter (fun x -> Printf.printf "%d " x) row;
      print_char '|')
    c;
  print_newline ()
