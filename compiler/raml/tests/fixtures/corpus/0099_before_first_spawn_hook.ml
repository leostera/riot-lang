(* Domain.before_first_spawn hook ordering. *)
let log = ref []

let () =
  Domain.before_first_spawn (fun () -> log := "first" :: !log);
  Domain.before_first_spawn (fun () -> log := "second" :: !log);
  let d = Domain.spawn (fun () -> 21) in
  ignore (Domain.join d);
  List.rev !log |> List.iter print_endline
