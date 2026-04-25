open Std
open Mime

let headers =
  [
    "Content-Type", "text/plain";
    "Content-Disposition", "attachment; filename=\"hello.txt\"";
    "Content-Transfer-Encoding", "base64";
  ]

let body = "SGVsbG8gV29ybGQ="

let main ~args:_ =
  match Mime.parse ~headers ~body with
  | Ok (SinglePart part) ->
      let filename = Mime.get_filename part |> Option.unwrap_or ~default:"<none>" in
      let decoded = Mime.get_decoded_content part |> Result.expect ~msg:"example content should decode" in
      println ("filename = " ^ filename);
      println ("content = " ^ decoded);
      Ok ()
  | Ok (MultiPart _) -> Error (Failure "expected a single MIME part")
  | Error err -> Error (Failure err)

let () = Runtime.run ~main ~args:Env.args ()
