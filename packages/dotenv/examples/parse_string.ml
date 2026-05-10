open Std

let source =
  "HOST=localhost
PORT=8080
DATABASE_URL=postgres://${HOST}:${PORT}/app
LITERAL='${HOST} is not substituted in single quotes'
"

let print_binding = fun (binding: Dotenv.binding) ->
  println
    (binding.key ^ "=" ^ binding.value ^ " (line " ^ Int.to_string binding.line ^ ")")

let main ~args:_ =
  match Dotenv.parse source with
  | Error error -> panic (Dotenv.error_to_string error)
  | Ok bindings ->
      List.for_each bindings ~fn:print_binding;
      Ok ()

let () = Runtime.run ~main ~args:Env.args ()
