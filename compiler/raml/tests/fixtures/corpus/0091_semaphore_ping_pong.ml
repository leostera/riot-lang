(* Blocking synchronization with binary semaphores. *)
let ping = Semaphore.Binary.make true
let pong = Semaphore.Binary.make false
let out = Array.make 6 ""

let worker label mine other parity =
  for i = 0 to 2 do
    Semaphore.Binary.acquire mine;
    out.((2 * i) + parity) <- Printf.sprintf "%s%d" label i;
    Semaphore.Binary.release other
  done

let child =
  Domain.spawn (fun () -> worker "pong" pong ping 1)

let () =
  worker "ping" ping pong 0;
  Domain.join child;
  Array.iter (fun s -> Printf.printf "%s " s) out;
  print_newline ()
