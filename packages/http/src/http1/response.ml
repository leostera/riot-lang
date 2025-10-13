(** HTTP/1.1 Response Parser *)

open Std
open Std.Iter
open Common

let (let*) = Result.and_then

type t = Std.Net.Http.Response.t

let parse_status_line input =
  let cursor = Cursor.create input in
  
  match Cursor.take_until cursor (fun c -> c = '\r') with
  | None -> Need_more
  | Some (line, cursor) ->
      match Cursor.advance_by cursor 2 with
      | None -> Error "Invalid line ending"
      | Some cursor ->
          let line_cursor = Cursor.create line in
          
          (* Get version *)
          match Cursor.take_until line_cursor (fun c -> c = ' ') with
          | None -> Error "Missing version"
          | Some (version, line_cursor) ->
              let line_cursor = Cursor.skip_while line_cursor (fun c -> c = ' ') in
              
              (* Get status code *)
              match Cursor.take_until line_cursor (fun c -> c = ' ') with
              | None -> Error "Missing status code"
              | Some (code_str, line_cursor) ->
                  let line_cursor = Cursor.skip_while line_cursor (fun c -> c = ' ') in
                  
                  (* Get reason phrase *)
                  let reason = Cursor.remaining line_cursor in
                  
                  (* Validate HTTP version prefix *)
                  let version_cursor = Cursor.create version in
                  match Cursor.take_n version_cursor 5 with
                  | Some ("HTTP/", _) ->
                      (match int_of_string_opt code_str with
                       | None -> Error "Invalid status code"
                       | Some status_code ->
                           Done {
                             value = (version, status_code, reason);
                             remaining = Cursor.remaining cursor
                           })
                  | _ -> Error "Invalid HTTP version"

let parse input =
  match parse_status_line input with
  | Need_more -> Need_more
  | Error e -> Error e
  | Done { value = (version_str, status_code, reason); remaining } ->
      let cursor = Cursor.create remaining in
      match Request.parse_headers cursor with
      | Need_more -> Need_more
      | Error e -> Error e
      | Done { value = (headers_list, body_start); _ } ->
          (* Build Std.Net.Http.Response.t *)
          let version = Std.Net.Http.Version.of_string version_str |> Result.unwrap_or ~default:Std.Net.Http.Version.Http11 in
          let status = Std.Net.Http.Status.of_int status_code in
          let headers = Std.Net.Http.Header.of_list headers_list in
          
          let body_cursor = Cursor.create body_start in
          let response = 
            Std.Net.Http.Response.create status
            |> (fun res -> Std.Net.Http.Response.with_version res version)
            |> (fun res -> Std.Net.Http.Response.with_headers res headers)
            |> (fun res -> if Cursor.length_remaining body_cursor > 0 then Std.Net.Http.Response.with_body res body_start else res)
          in
          
          Done { value = response; remaining = "" }
