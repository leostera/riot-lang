(* Domain-local storage. *)
let key = Domain.DLS.new_key (fun () -> 0)

let worker start =
  Domain.DLS.set key start;
  let x = Domain.DLS.get key in
  Domain.DLS.set key (x + 1);
  Domain.DLS.get key

let child = Domain.spawn (fun () -> worker 10)

let () =
  let main_before = worker 0 in
  let child_value = Domain.join child in
  let main_after = Domain.DLS.get key in
  Printf.printf "%d %d %d\n" main_before child_value main_after
