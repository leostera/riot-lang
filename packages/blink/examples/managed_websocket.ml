open Std

module Client = Blink.Client

let main ~args =
  let url =
    Env.var Env.String ~name:"BLINK_WS_URL"
    |> Option.unwrap_or ~default:"wss://echo.websocket.events"
  in
  let uri =
    Net.Uri.from_string url
    |> Result.expect ~msg:"invalid WebSocket URL"
  in
  let client = Client.make () in
  let websocket =
    match Client.WebSocket.connect client uri with
    | Ok websocket -> websocket
    | Error _ -> panic "managed WebSocket connect failed"
  in
  Client.WebSocket.send_text client websocket "hello from Blink.Client"
  |> Result.expect ~msg:"managed WebSocket send failed";
  (
    match Client.WebSocket.receive client websocket with
    | Ok (Client.WebSocket.Text payload) -> println ("text: " ^ payload)
    | Ok (Client.WebSocket.Binary payload) ->
        println ("binary bytes=" ^ Int.to_string (String.length payload))
    | Ok (Client.WebSocket.Ping _) -> println "ping"
    | Ok (Client.WebSocket.Pong _) -> println "pong"
    | Ok (Client.WebSocket.Close _) -> println "closed"
    | Error _ -> println "receive failed"
  );
  Client.WebSocket.close client websocket;
  Client.shutdown client;
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
