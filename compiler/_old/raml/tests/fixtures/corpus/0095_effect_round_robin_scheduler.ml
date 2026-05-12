(* Cooperative scheduler with effects. *)
open Effect
open Effect.Deep

type _ Effect.t +=
  | Fork : (unit -> unit) -> unit t
  | Yield : unit t

let fork f = perform (Fork f)
let yield () = perform Yield

let run main =
  let q : (unit -> unit) Queue.t = Queue.create () in
  let enqueue task = Queue.push task q in
  let rec dequeue () =
    if Queue.is_empty q then ()
    else
      let task = Queue.pop q in
      spawn task
  and spawn f =
    match f () with
    | () -> dequeue ()
    | effect Yield, k ->
        enqueue (fun () -> continue k ());
        dequeue ()
    | effect (Fork f2), k ->
        enqueue (fun () -> continue k ());
        spawn f2
  in
  spawn main

let log = ref []

let emit s = log := s :: !log

let task name count () =
  let rec loop i =
    if i > count then ()
    else begin
      emit (Printf.sprintf "%s%d" name i);
      yield ();
      loop (i + 1)
    end
  in
  loop 1

let () =
  run (fun () ->
      fork (task "b" 3);
      task "a" 3 ());
  List.rev !log |> List.iter (fun s -> Printf.printf "%s " s);
  print_newline ()
