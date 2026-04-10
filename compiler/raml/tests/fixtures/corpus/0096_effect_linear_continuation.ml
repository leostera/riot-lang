(* Continuations are linear and cannot be resumed twice. *)
open Effect
open Effect.Deep

type _ Effect.t += Ping : int t

let comp () =
  ignore (perform Ping)

let () =
  let message =
    try
      (try comp () with
       | effect Ping, k ->
           ignore (continue k 1);
           ignore (continue k 2));
      "ok"
    with
    | Effect.Continuation_already_resumed -> "already_resumed"
  in
  print_endline message
