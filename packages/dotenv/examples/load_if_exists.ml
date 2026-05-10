open Std

let print_loaded = fun (binding: Dotenv.binding) ->
  println
    ("loaded " ^ binding.key ^ "=" ^ binding.value)

let main ~args:_ =
  match Dotenv.load_if_exists ~env:"local" () with
  | Error error -> panic (Dotenv.error_to_string error)
  | Ok [] ->
      println "No .env or .env.local file was present.";
      Ok ()
  | Ok bindings ->
      List.for_each bindings ~fn:print_loaded;
      Ok ()

let () = Runtime.run ~main ~args:Env.args ()
