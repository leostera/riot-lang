open Std
open Jsonrpc

let main ~args:_ =
  let request = Jsonrpc.request ~method_:"ping" ~params:(
    Named [
      "client", Data.Json.string "riot";
    ]
  ) ~id:(Number 1) () in
  println (Data.Json.to_string_pretty (Jsonrpc.request_to_json request));
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
