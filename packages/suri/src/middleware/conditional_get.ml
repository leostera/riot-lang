open Std

type date_parse_error =
  | InvalidDateFormat of { value: string }
  | InvalidDay of { value: string }
  | InvalidMonth of { value: string }
  | InvalidYear of { value: string }
  | InvalidTimeFormat of { value: string }
  | InvalidHour of { value: string }
  | InvalidMinute of { value: string }
  | InvalidSecond of { value: string }

type modified_since_error =
  | InvalidRequestDate of date_parse_error
  | InvalidResponseDate of date_parse_error

let date_parse_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidDateFormat { value } -> "invalid HTTP date format: " ^ value
  | InvalidDay { value } -> "invalid HTTP date day: " ^ value
  | InvalidMonth { value } -> "invalid HTTP date month: " ^ value
  | InvalidYear { value } -> "invalid HTTP date year: " ^ value
  | InvalidTimeFormat { value } -> "invalid HTTP date time format: " ^ value
  | InvalidHour { value } -> "invalid HTTP date hour: " ^ value
  | InvalidMinute { value } -> "invalid HTTP date minute: " ^ value
  | InvalidSecond { value } -> "invalid HTTP date second: " ^ value

let modified_since_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidRequestDate error ->
      "invalid If-Modified-Since request date: " ^ date_parse_error_to_string error
  | InvalidResponseDate error ->
      "invalid Last-Modified response date: " ^ date_parse_error_to_string error

let parse_int_field = fun error value ->
  match Int.from_string_opt value with
  | Some parsed -> Ok parsed
  | None -> Error (error value)

let month_from_string = fun value ->
  match String.lowercase_ascii value with
  | "jan" -> Ok 0
  | "feb" -> Ok 1
  | "mar" -> Ok 2
  | "apr" -> Ok 3
  | "may" -> Ok 4
  | "jun" -> Ok 5
  | "jul" -> Ok 6
  | "aug" -> Ok 7
  | "sep" -> Ok 8
  | "oct" -> Ok 9
  | "nov" -> Ok 10
  | "dec" -> Ok 11
  | _ -> Error (InvalidMonth { value })

(** Parse HTTP date format (RFC 7231) into Unix timestamp *)
let parse_http_date = fun date_str ->
  let parts = String.split_on_char ' ' date_str in
  match parts with
  | [ _day; day; month; year; time; _tz ] -> (
      match parse_int_field (fun value -> InvalidDay { value }) day with
      | Error error -> Error error
      | Ok day -> (
          match month_from_string month with
          | Error error -> Error error
          | Ok month -> (
              match parse_int_field (fun value -> InvalidYear { value }) year with
              | Error error -> Error error
              | Ok year -> (
                  match String.split_on_char ':' time with
                  | [ hour; minute; second ] -> (
                      match parse_int_field (fun value -> InvalidHour { value }) hour with
                      | Error error -> Error error
                      | Ok hours -> (
                          match parse_int_field (fun value -> InvalidMinute { value }) minute with
                          | Error error -> Error error
                          | Ok minutes -> (
                              match parse_int_field (fun value -> InvalidSecond { value }) second with
                              | Error error -> Error error
                              | Ok seconds ->
                                  (* Simplified Unix timestamp calculation. *)
                                  let years_since_1970 = year - 1_970 in
                                  let leap_years =
                                    (years_since_1970 / 4) - (years_since_1970 / 100)
                                    + (years_since_1970 / 400)
                                  in
                                  let days_from_years = (years_since_1970 * 365) + leap_years in
                                  let is_leap =
                                    (year mod 4 = 0 && year mod 100 != 0) || year mod 400 = 0
                                  in
                                  let days_in_month = [
                                    31;
                                    if is_leap then
                                      29
                                    else
                                      28;
                                    31;
                                    30;
                                    31;
                                    30;
                                    31;
                                    31;
                                    30;
                                    31;
                                    30;
                                    31;
                                  ]
                                  in
                                  let days_from_months =
                                    List.take days_in_month ~len:month
                                    |> List.fold_left ~fn:(fun acc days -> acc + days) ~init:0
                                  in
                                  let days_since_epoch =
                                    days_from_years + days_from_months + day - 1
                                  in
                                  let total_seconds =
                                    (days_since_epoch * 86_400)
                                    + (hours * 3_600)
                                    + (minutes * 60)
                                    + seconds
                                  in
                                  Ok (Float.from_int total_seconds)
                            )
                        )
                    )
                  | _ -> Error (InvalidTimeFormat { value = time })
                )
            )
        )
    )
  | _ -> Error (InvalidDateFormat { value = date_str })

(** Check if ETag matches If-None-Match header *)
let check_etag_match = fun conn resp_headers ->
  let req_headers = Conn.headers conn in
  let resp_etag = Net.Http.Header.get resp_headers "etag" in
  match (Net.Http.Header.get req_headers "if-none-match", resp_etag) with
  | (Some client_etag, Some server_etag) ->
      (* Handle multiple ETags in If-None-Match (comma-separated) *)
      let client_etags =
        String.split_on_char ',' client_etag
        |> List.map ~fn:String.trim
      in
      (* Check for wildcard match *)
      if List.contains client_etags ~value:"*" then
        true
      else
        (* Check if server ETag matches any client ETag *)
        List.contains client_etags ~value:server_etag
  | _ -> false

(** Check if Last-Modified matches If-Modified-Since header *)
let check_modified_since = fun conn resp_headers ->
  let req_headers = Conn.headers conn in
  let last_modified = Net.Http.Header.get resp_headers "last-modified" in
  match (Net.Http.Header.get req_headers "if-modified-since", last_modified) with
  | (Some client_date_str, Some server_date_str) -> (
      match (parse_http_date client_date_str, parse_http_date server_date_str) with
      | (Ok client_time, Ok server_time) ->
          (* Not modified if server time <= client time *)
          Ok (server_time <= client_time)
      | (Error error, _) -> Error (InvalidRequestDate error)
      | (_, Error error) -> Error (InvalidResponseDate error)
    )
  | _ -> Ok false

(** Headers to preserve in 304 responses *)
let cacheable_headers = [
  "cache-control";
  "content-location";
  "date";
  "etag";
  "expires";
  "vary";
  "last-modified";
]

(** Build 304 response with preserved headers *)
let not_modified_response = fun conn resp_headers ->
  (* Start with 304 status and empty body *)
  let conn' = Conn.with_status Net.Http.Status.NotModified conn in
  let conn' = Conn.with_body "" conn' in
  (* Preserve cacheable headers from response *)
  List.fold_left
    cacheable_headers
    ~init:conn'
    ~fn:(fun acc_conn header_name ->
      match Net.Http.Header.get resp_headers header_name with
      | Some value -> Conn.with_header header_name value acc_conn
      | None -> acc_conn)

(** Conditional GET middleware *)
let middleware = fun ~conn ~next ->
  (* Only apply to GET and HEAD requests *)
  let method_ = Conn.method_ conn in
  match method_ with
  | Net.Http.Method.Get
  | Net.Http.Method.Head ->
      (* Process request *)
      let conn' = next conn in
      (* Get response headers *)
      let resp_headers =
        List.fold_left
          (Conn.resp_headers conn')
          ~init:Net.Http.Header.empty
          ~fn:(fun headers (name, value) ->
            Net.Http.Header.add headers name value)
      in
      (* Check if we should return 304 *)
      let req_headers = Conn.headers conn in
      let has_if_none_match = Option.is_some (Net.Http.Header.get req_headers "if-none-match") in
      let etag_matches = check_etag_match conn resp_headers in
      let modified_since_matches =
        match check_modified_since conn resp_headers with
        | Ok matched -> matched
        | Error _ -> false
      in
      if etag_matches || ((not has_if_none_match) && modified_since_matches) then
        not_modified_response conn' resp_headers
      else
        (* Return full response *)
        conn'
  | _ ->
      (* Other methods pass through unchanged *)
      next conn
