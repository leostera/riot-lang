open Std

(** Parse HTTP date format (RFC 7231) into Unix timestamp *)
let parse_http_date = fun date_str ->
  try
    let parts = String.split_on_char ' ' date_str in
    match parts with
    | [_day;day;month;year;time;_tz] ->
        let day = Int.of_string day in
        let year = Int.of_string year in
        let month =
          match String.lowercase_ascii month with
          | "jan" -> 0
          | "feb" -> 1
          | "mar" -> 2
          | "apr" -> 3
          | "may" -> 4
          | "jun" -> 5
          | "jul" -> 6
          | "aug" -> 7
          | "sep" -> 8
          | "oct" -> 9
          | "nov" -> 10
          | "dec" -> 11
          | _ -> raise (Invalid_argument "Invalid month")
        in
        let time_parts = String.split_on_char ':' time in
        let get_time_part index =
          match List.get time_parts ~at:index with
          | Option.Some v -> Int.of_string v
          | Option.None -> raise (Invalid_argument "Invalid time format")
        in
        let hours = get_time_part 0 in
        let minutes = get_time_part 1 in
        let seconds = get_time_part 2 in
        (* Simplified Unix timestamp calculation *)
        (* This is approximate - proper implementation would use Time module *)
        let days_since_epoch =
          (* Days from 1970 to start of year *)
          let years_since_1970 = year - 1_970 in
          let leap_years = (years_since_1970 / 4) - (years_since_1970 / 100)
          + (years_since_1970 / 400) in
          let days_from_years = (years_since_1970 * 365) + leap_years in
          (* Days in months *)
          let is_leap = (year mod 4 = 0 && year mod 100 != 0) || year mod 400 = 0 in
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
            31
          ]
          in
          let days_from_months = List.take days_in_month ~len:month
          |> List.fold_left ~fn:(fun acc days -> acc + days) ~init:0 in
          days_from_years + days_from_months + day - 1
        in
        let total_seconds = (days_since_epoch * 86_400) + (hours * 3_600) + (minutes * 60) + seconds in
        Some (Float.of_int total_seconds)
    | _ -> None
  with
  | _ -> None

(** Check if ETag matches If-None-Match header *)
let check_etag_match = fun conn resp_headers ->
  let req_headers = Conn.headers conn in
  let resp_etag = Net.Http.Header.get resp_headers "etag" in
  match (Net.Http.Header.get req_headers "if-none-match", resp_etag) with
  | Some client_etag, Some server_etag ->
      (* Handle multiple ETags in If-None-Match (comma-separated) *)
      let client_etags = String.split_on_char ',' client_etag |> List.map ~fn:String.trim in
      (* Check for wildcard match *)
      if List.mem "*" client_etags then
        true
      else
        (* Check if server ETag matches any client ETag *)
        List.mem server_etag client_etags
  | _ -> false

(** Check if Last-Modified matches If-Modified-Since header *)
let check_modified_since = fun conn resp_headers ->
  let req_headers = Conn.headers conn in
  let last_modified = Net.Http.Header.get resp_headers "last-modified" in
  match (Net.Http.Header.get req_headers "if-modified-since", last_modified) with
  | Some client_date_str, Some server_date_str -> (
      match (parse_http_date client_date_str, parse_http_date server_date_str) with
      | Some client_time, Some server_time ->
          (* Not modified if server time <= client time *)
          server_time <= client_time
      | _ -> false
    )
  | _ -> false

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
  List.fold_left cacheable_headers ~init:conn'
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
        List.fold_left (Conn.resp_headers conn') ~init:Net.Http.Header.empty
          ~fn:(fun headers ((name, value)) ->
            Net.Http.Header.add headers name value)
      in
      (* Check if we should return 304 *)
      let etag_matches = check_etag_match conn resp_headers in
      let not_modified = check_modified_since conn resp_headers in
      if etag_matches || not_modified then
        not_modified_response conn' resp_headers
      else
        (* Return full response *)
        conn'
  | _ ->
      (* Other methods pass through unchanged *)
      next conn
