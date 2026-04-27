open Std

let hex_digit = fun value -> String.get_unchecked "0123456789ABCDEF" ~at:value

let percent_encode_byte = fun code ->
  "%"
  ^ String.make ~len:1 ~char:(hex_digit ((code lsr 4) land 0xf))
  ^ String.make ~len:1 ~char:(hex_digit (code land 0xf))

let sanitize_path = fun path ->
  let buf = IO.Buffer.create ~size:(String.length path) in
  String.iter
    (fun char ->
      let code = Char.to_int char in
      if code < 0x20 || code = 0x7f then
        IO.Buffer.add_string buf (percent_encode_byte code)
      else
        IO.Buffer.add_char buf char)
    path;
  IO.Buffer.contents buf

let logger = fun ~conn ~next ->
  let start = Time.Instant.now () in
  let method_str =
    Conn.method_ conn
    |> Net.Http.Method.to_string
  in
  let path =
    Conn.path conn
    |> sanitize_path
  in
  (* Call next middleware/handler *)
  let conn' = next conn in
  (* Calculate duration and get response info *)
  let duration = Time.Instant.elapsed start in
  let duration_ms = Time.Duration.to_millis duration in
  let response = Conn.to_response conn' in
  let status =
    response.Web_server.Response.status
    |> Net.Http.Status.to_int
  in
  (* Format duration: use µs if < 1ms, otherwise ms *)
  let duration_str =
    if duration_ms < 1 then
      let duration_us = Time.Duration.to_micros duration in
      Int.to_string duration_us ^ "µs"
    else
      Int.to_string duration_ms ^ "ms"
  in
  (* Build log message *)
  let msg = method_str ^ " " ^ path ^ " -> " ^ Int.to_string status ^ " in " ^ duration_str in
  (* Log at appropriate level based on status and duration *)
  if status >= 500 then
    Log.error msg
  else if status >= 400 then
    Log.warn msg
  else if duration_ms >= 1_000 then
    Log.warn msg
  else
    Log.info msg;
  conn'
