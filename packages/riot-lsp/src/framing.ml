open Std
open Std.Result.Syntax

type reader = (unit, IO.error) IO.Reader.t
type writer = (unit, IO.error) IO.Writer.t

let strip_line_ending = fun line ->
  let len = String.length line in
  if len >= 2 && String.sub line ~offset:(len - 2) ~len:2 = "\r\n" then
    String.sub line ~offset:0 ~len:(len - 2)
  else if len >= 1 && String.get line ~at:(len - 1) = Some '\n' then
    String.sub line ~offset:0 ~len:(len - 1)
  else
    line

let header_separator = fun payload ->
  let len = String.length payload in
  let rec loop index =
    if index + 3 < len && String.sub payload ~offset:index ~len:4 = "\r\n\r\n" then
      Some (index, 4)
    else if index + 1 < len && String.sub payload ~offset:index ~len:2 = "\n\n" then
      Some (index, 2)
    else if index >= len then
      None
    else
      loop (index + 1)
  in
  loop 0

let parse_content_length = fun headers ->
  let parse_int = Int.parse in
  let rec loop = function
    | [] -> Error "missing Content-Length header"
    | header :: rest -> (
        match String.split_on_char ':' header with
        | name :: value_parts when String.equal (String.lowercase_ascii (String.trim name)) "content-length" -> (
            match parse_int (String.trim (String.concat ":" value_parts)) with
            | Some length when length >= 0 -> Ok length
            | Some _ -> Error "Content-Length must be non-negative"
            | None -> Error "invalid Content-Length header"
          )
        | _ -> loop rest
      )
  in
  loop headers

let encode = fun payload ->
  "Content-Length: " ^ Int.to_string (String.length payload) ^ "\r\n\r\n" ^ payload

let decode_one = fun framed ->
  match header_separator framed with
  | None -> Error "missing header/body separator"
  | Some (header_end, separator_len) ->
      let headers = String.sub framed ~offset:0 ~len:header_end
      |> String.split_on_char '\n'
      |> List.map ~fn:strip_line_ending
      |> List.filter ~fn:(fun line -> not (String.equal line "")) in
      let payload_start = header_end + separator_len in
      let body =
        String.sub framed ~offset:payload_start ~len:(String.length framed - payload_start)
      in
      let* content_length = parse_content_length headers in
      if String.length body < content_length then
        Error "payload shorter than Content-Length"
      else
        let payload = String.sub body ~offset:0 ~len:content_length in
        let rest =
          String.sub body ~offset:content_length ~len:(String.length body - content_length)
        in
        Ok (payload, rest)

let read = fun input ->
  let read_line = fun () ->
    let chunk = IO.Bytes.create ~size:1 in
    let buffer = IO.Buffer.create ~size:256 in
    let rec loop () =
      let* read = IO.Reader.read input chunk |> Result.map_err ~fn:IO.error_message in
      if read = 0 then
        Ok (IO.Buffer.contents buffer)
      else if not (Int.equal read 1) then
        Error "unexpected short read while reading LSP headers"
      else (
        let char = IO.Bytes.get_unchecked chunk ~at:0 in
        IO.Buffer.add_char buffer char;
        if Char.equal char '\n' then
          Ok (IO.Buffer.contents buffer)
        else
          loop ())
    in
    loop ()
  in
  let read_exact = fun content_length ->
    let buffer = IO.Bytes.create ~size:content_length in
    let chunk_size = 4_096 in
    let rec loop offset remaining =
      if remaining = 0 then
        Ok ()
      else
        let to_read = Int.min remaining chunk_size in
        let chunk = IO.Bytes.create ~size:to_read in
        let* read = IO.Reader.read input chunk |> Result.map_err ~fn:IO.error_message in
        if read = 0 then
          Error "unexpected EOF while reading LSP payload"
        else
          (
            IO.Bytes.blit_unchecked chunk ~src_offset:0 ~dst:buffer ~dst_offset:offset ~len:read;
            loop (offset + read) (remaining - read))
    in
    let* () = loop 0 content_length in
    Ok (IO.Bytes.to_string buffer)
  in
  let rec read_headers acc =
    let* raw_line = read_line () in
    if String.equal raw_line "" then
        if List.is_empty acc then
          Ok None
        else
          Error "unexpected EOF while reading LSP headers"
    else
      let line = strip_line_ending raw_line in
      if String.equal line "" then
        Ok (Some (List.rev acc))
      else
        read_headers (line :: acc)
  in
  let* headers_opt = read_headers [] in
  match headers_opt with
  | None -> Ok None
  | Some headers ->
  let* content_length = parse_content_length headers in
  let* payload = read_exact content_length in
  Ok (Some payload)

let write = fun output payload ->
  IO.Writer.write_all output (encode payload) |> Result.map_err ~fn:IO.error_message
