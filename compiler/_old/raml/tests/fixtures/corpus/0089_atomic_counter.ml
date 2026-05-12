(* Atomic increments from multiple domains. *)
let counter = Atomic.make 0

let worker reps =
  for _ = 1 to reps do
    Atomic.incr counter
  done

let domains =
  Array.init 4 (fun _ -> Domain.spawn (fun () -> worker 10000))

let () =
  Array.iter Domain.join domains;
  Printf.printf "%d\n" (Atomic.get counter)
