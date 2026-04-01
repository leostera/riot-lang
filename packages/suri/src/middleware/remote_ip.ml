open Std
(** Check if IP string is in trusted proxy list *)
let is_trusted_proxy = fun proxies ip_str ->
  List.exists (String.equal ip_str) proxies
(** Extract IPs from X-Forwarded-For header *)
let parse_forwarded_for = fun header_value ->
  String.split_on_char ',' header_value
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)
(** Find real client IP by walking X-Forwarded-For chain from right to left *)
let find_real_ip = fun proxies forwarded_ips ->
  (* Walk from right to left (closest proxy first) *)
  let rec walk_chain = function
    | [] -> None
    | ip_str :: rest ->
        (* If this IP is trusted, skip it and continue *)
        if is_trusted_proxy proxies ip_str then
          walk_chain rest
        else
          (* First untrusted IP = real client *)
          Some ip_str
  in
  walk_chain (List.rev forwarded_ips)
(** Remote IP middleware *)
let middleware = fun ?(header = "x-forwarded-for") () ~proxies ~conn ~next ->
  (* If no proxies configured, pass through unchanged (safe default) *)
  if List.length proxies = 0 then
    next conn
  else
    (* Get the header value *)
    let headers = Conn.headers conn in
    match Net.Http.Header.get headers header with
    | None ->
        (* No header present, use existing peer *)
        next conn
    | Some header_value -> (
        (* Parse IPs from header *)
        let forwarded_ips = parse_forwarded_for header_value in
        match find_real_ip proxies forwarded_ips with
        | Some real_ip_str ->
            (* Update peer with real client IP (tcp_addr is just a string) *)
            let current_peer = Conn.peer conn in
            let new_peer = { current_peer with ip = real_ip_str } in
            let conn' = Conn.with_peer new_peer conn in
            next conn'
        | None ->
            (* Couldn't determine real IP, use existing peer *)
            next conn
      )
