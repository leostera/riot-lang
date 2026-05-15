open Std

module Client = Blink.Client

let main ~args =
  let url =
    Env.var Env.String ~name:"BLINK_SSE_URL"
    |> Option.unwrap_or ~default:"https://example.com/events"
  in
  let uri =
    Net.Uri.from_string url
    |> Result.expect ~msg:"invalid SSE URL"
  in
  let client = Client.make () in
  let conn =
    match Client.connect client uri with
    | Ok conn -> conn
    | Error _ -> panic "managed SSE connect failed"
  in
  let request =
    let request = Net.Http.Request.create Net.Http.Method.Get uri in
    Net.Http.Request.with_header request "Accept" "text/event-stream"
  in
  Client.request client conn request ()
  |> Result.expect ~msg:"managed SSE request failed";
  let events = Client.SSE.await client conn in
  let rec consume remaining =
    if remaining <= 0 then
      ()
    else
      match Iter.MutIterator.next events with
      | None -> ()
      | Some (Ok event) ->
          println ("event: " ^ event.data);
          consume (remaining - 1)
      | Some (Error error) -> panic ("managed SSE stream failed: " ^ Blink.Error.to_string error)
  in
  consume 5;
  Client.close client conn;
  Client.shutdown client;
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
