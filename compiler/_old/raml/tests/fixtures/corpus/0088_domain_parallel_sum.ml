(* Parallel decomposition with multiple domains. *)
let chunk_sum start stop =
  let rec loop acc i =
    if i > stop then acc else loop (acc + i) (i + 1)
  in
  loop 0 start

let d1 = Domain.spawn (fun () -> chunk_sum 1 20000)
let d2 = Domain.spawn (fun () -> chunk_sum 20001 40000)

let () =
  let total = Domain.join d1 + Domain.join d2 in
  Printf.printf "%d\n" total
