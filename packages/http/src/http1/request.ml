(** HTTP/1.1 Request Parser *)

open Std
open Std.Iter
open Common

let ( let* ) = Result.and_then

let parse_request_line ?(max_length = 8192) input =
  let cursor = Cursor.create input in

  (* Take line until \r *)
  match Cursor.take_until cursor (fun c -> c = '\r') with
  | None -> Need_more
  | Some (line, cursor) -> (
      let line_cursor = Cursor.create line in
      if Cursor.length_remaining line_cursor > max_length then
        Error "Request line too long"
      else
        (* Skip \r\n *)
        match Cursor.advance_by cursor 2 with
        | None -> Error "Invalid line ending"
        | Some cursor -> (
            (* Parse the line: METHOD SP path SP HTTP/version *)
            let line_cursor = Cursor.create line in

            (* Get method *)
            match Cursor.take_until line_cursor (fun c -> c = ' ') with
            | None -> Error "Missing method"
            | Some (method_str, line_cursor) -> (
                let line_cursor =
                  Cursor.skip_while line_cursor (fun c -> c = ' ')
                in

                (* Get path *)
                match Cursor.take_until line_cursor (fun c -> c = ' ') with
                | None -> Error "Missing path"
                | Some (path, line_cursor) -> (

                    let line_cursor =
                      Cursor.skip_while line_cursor (fun c -> c = ' ')
                    in

                    (* Get version *)
                    let version_str = Cursor.remaining line_cursor in
                    let version_cursor = Cursor.create version_str in

                    match Cursor.take_n version_cursor 5 with
                    | Some ("HTTP/", _) ->
                        Done
                          {
                            value = (method_str, path, version_str);
                            remaining = Cursor.remaining cursor;
                          }
                    | _ -> Error "Invalid HTTP version"))))

let parse_header_line cursor =
  match Cursor.take_until cursor (fun c -> c = '\r') with
  | None -> Need_more
  | Some (line, cursor) -> (
      match Cursor.advance_by cursor 2 with
      | None -> Error "Invalid line ending"
      | Some cursor -> (
          let line_cursor = Cursor.create line in

          match Cursor.take_until line_cursor (fun c -> c = ':') with
          | None -> Error "Invalid header format (missing colon)"
          | Some (name, line_cursor) -> (
              (* Skip the colon and optional whitespace *)
              match Cursor.advance line_cursor with
              | None -> Error "Invalid header format"
              | Some line_cursor ->
                  let line_cursor =
                    Cursor.skip_while line_cursor (fun c -> c = ' ' || c = '\t')
                  in
                  let value = Cursor.remaining line_cursor in
                  (* Trim name *)
                  let name_cursor = Cursor.create name in
                  let name_cursor =
                    Cursor.skip_while name_cursor (fun c -> c = ' ' || c = '\t')
                  in
                  let name = Cursor.remaining name_cursor in
                  Done
                    {
                      value = (name, value);
                      remaining = Cursor.remaining cursor;
                    })))

let rec parse_headers ?(max_count = 100) ?(max_length = 8192) ?(acc = []) cursor
    =
  match Cursor.take_n cursor 2 with
  | Some ("\r\n", cursor) ->
      (* End of headers *)
      Done { value = (List.rev acc, Cursor.remaining cursor); remaining = "" }
  | _ -> (
      if List.length acc >= max_count then Error "Too many headers"
      else
        match parse_header_line cursor with
        | Need_more -> Need_more
        | Error e -> Error e
        | Done { value = name, value; remaining } ->
            let name_cursor = Cursor.create name in
            let value_cursor = Cursor.create value in
            if
              Cursor.length_remaining name_cursor
              + Cursor.length_remaining value_cursor
              > max_length
            then Error "Header too long"
            else
              let cursor = Cursor.create remaining in
              parse_headers ~max_count ~max_length ~acc:((name, value) :: acc)
                cursor)

let parse ?(max_request_line = 8192) ?(max_headers = 100)
    ?(max_header_length = 8192) input =
  match parse_request_line ~max_length:max_request_line input with
  | Need_more -> Need_more
  | Error e -> Error e
  | Done { value = method_str, path_str, version_str; remaining } -> (
      let cursor = Cursor.create remaining in
      match
        parse_headers ~max_count:max_headers ~max_length:max_header_length
          cursor
      with
      | Need_more -> Need_more
      | Error e -> Error e
      | Done { value = headers_list, body_start; _ } ->
          (* Build Std.Net.Http.Request.t *)
          let method_ = Std.Net.Http.Method.of_string method_str in
          let uri =
            Std.Net.Uri.of_string path_str
            |> Result.unwrap_or
                 ~default:(Std.Net.Uri.of_string "/" |> Result.unwrap)
          in
          let version =
            Std.Net.Http.Version.of_string version_str
            |> Result.unwrap_or ~default:Std.Net.Http.Version.Http11
          in
          let headers = Std.Net.Http.Header.of_list headers_list in

          let body_cursor = Cursor.create body_start in
          let request =
            ( ( Std.Net.Http.Request.create method_ uri |> fun req ->
                Std.Net.Http.Request.with_version req version )
            |> fun req -> Std.Net.Http.Request.with_headers req headers )
            |> fun req ->
            if Cursor.length_remaining body_cursor > 0 then
              Std.Net.Http.Request.with_body req body_start
            else req
          in

          Done { value = request; remaining = body_start })
