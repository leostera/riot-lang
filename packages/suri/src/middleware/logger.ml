open Std

let logger = fun ~conn ~next ->
  let start = Time.Instant.now () in
  let method_str = Conn.method_ conn |> Net.Http.Method.to_string in
  let path = Conn.path conn in
  (* Call next middleware/handler *)
  let conn' = next conn in
  (* Calculate duration and get response info *)
  let duration = Time.Instant.elapsed start in
  let duration_ms = Time.Duration.to_millis duration in
  let response = Conn.to_response conn' in
  let status = response.Web_server.Response.status |> Net.Http.Status.to_int in
  (* Format duration: use µs if < 1ms, otherwise ms *)
  let duration_str =
    if duration_ms < 1 then
      let duration_us = Time.Duration.to_micros duration in Int.to_string duration_us ^ "µs"
    else Int.to_string duration_ms ^ "ms"
  in
  (* Build log message *)
  let msg = method_str ^ " " ^ path ^ " -> " ^ Int.to_string status ^ " in " ^ duration_str in
  (* Log at appropriate level based on status and duration *)
  if status >= 500 then
    Log.error msg
  else
    if status >= 400 then
      Log.warn msg
    else
      if duration_ms >= 1_000 then
        Log.warn msg
      else Log.info msg;
  conn'
