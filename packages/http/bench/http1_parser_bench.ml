open Std
open Http

module BaselineParser = struct
  open Http1.Common

  module StringCursor = struct
    type t = { source: string; pos: int; length: int }

    let create = fun source -> { source; pos = 0; length = String.length source }

    let is_eof = fun cursor -> cursor.pos >= cursor.length

    let advance = fun cursor ->
      if is_eof cursor then
        None
      else
        Some { cursor with pos = cursor.pos + 1 }

    let advance_by = fun cursor count ->
      let pos = cursor.pos + count in
      if pos > cursor.length then
        None
      else
        Some { cursor with pos }

    let take_until = fun cursor predicate ->
      let start = cursor.pos in
      let rec loop pos =
        if pos >= cursor.length then
          None
        else if predicate (String.get_unchecked cursor.source ~at:pos) then
          Some pos
        else
          loop (pos + 1)
      in
      match loop start with
      | None -> None
      | Some stop ->
          Some (
            String.sub cursor.source ~offset:start ~len:(stop - start),
            { cursor with pos = stop }
          )

    let take_n = fun cursor count ->
      if cursor.pos + count > cursor.length then
        None
      else
        Some (
          String.sub cursor.source ~offset:cursor.pos ~len:count,
          { cursor with pos = cursor.pos + count }
        )

    let take_while = fun cursor predicate ->
      let start = cursor.pos in
      let rec loop pos =
        if pos >= cursor.length then
          pos
        else if predicate (String.get_unchecked cursor.source ~at:pos) then
          loop (pos + 1)
        else
          pos
      in
      let stop = loop start in
      (String.sub cursor.source ~offset:start ~len:(stop - start), { cursor with pos = stop })

    let skip_while = fun cursor predicate ->
      let (_, cursor) = take_while cursor predicate in
      cursor

    let remaining = fun cursor ->
      if is_eof cursor then
        ""
      else
        String.sub cursor.source ~offset:cursor.pos ~len:(cursor.length - cursor.pos)
  end

  let parse_request_line = fun ?(max_length = 8_192) input ->
    let cursor = StringCursor.create input in
    match StringCursor.take_until cursor (fun c -> c = '\r') with
    | None -> Need_more
    | Some (line, cursor) ->
        if String.length line > max_length then
          Error (RequestLineTooLong { max_length })
        else
          match StringCursor.advance_by cursor 2 with
          | None -> Error InvalidCrlf
          | Some cursor -> (
              let line_cursor = StringCursor.create line in
              match StringCursor.take_until line_cursor (fun c -> c = ' ') with
              | None -> Error MissingMethod
              | Some (method_, line_cursor) ->
                  let line_cursor = StringCursor.skip_while line_cursor (fun c -> c = ' ') in
                  match StringCursor.take_until line_cursor (fun c -> c = ' ') with
                  | None -> Error MissingPath
                  | Some (path, line_cursor) ->
                      let version =
                        StringCursor.skip_while line_cursor (fun c -> c = ' ')
                        |> StringCursor.remaining
                      in
                      if String.starts_with ~prefix:"HTTP/" version then
                        Done {
                          value = (method_, path, version);
                          remaining = StringCursor.remaining cursor;
                        }
                      else
                        Error InvalidHttpVersion
            )

  let parse_header_line = fun cursor ->
    match StringCursor.take_until cursor (fun c -> c = '\r') with
    | None -> Need_more
    | Some (line, cursor) -> (
        match StringCursor.advance_by cursor 2 with
        | None -> Error InvalidCrlf
        | Some cursor -> (
            let line_cursor = StringCursor.create line in
            match StringCursor.take_until line_cursor (fun c -> c = ':') with
            | None -> Error (InvalidHeaderFormat MissingColon)
            | Some (name, line_cursor) -> (
                match StringCursor.advance line_cursor with
                | None -> Error (InvalidHeaderFormat MissingValueSeparator)
                | Some line_cursor ->
                    let value =
                      StringCursor.skip_while line_cursor (fun c -> c = ' ' || c = '\t')
                      |> StringCursor.remaining
                    in
                    let name =
                      StringCursor.skip_while
                        (StringCursor.create name)
                        (fun c -> c = ' ' || c = '\t')
                      |> StringCursor.remaining
                    in
                    Done {
                      value = (name, value);
                      remaining = StringCursor.remaining cursor;
                    }
              )
          )
      )

  let rec parse_headers = fun ?(max_count = 100) ?(max_length = 8_192) ?(acc = []) cursor ->
    match StringCursor.take_n cursor 2 with
    | Some ("\r\n", cursor) ->
        Done {
          value = (List.rev acc, StringCursor.remaining cursor);
          remaining = "";
        }
    | _ ->
        if List.length acc >= max_count then
          Error (TooManyHeaders { max_count })
        else
          match parse_header_line cursor with
          | Need_more -> Need_more
          | Error error -> Error error
          | Done { value = (name, value); remaining } ->
              if String.length name + String.length value > max_length then
                Error (HeaderTooLong { max_length })
              else
                parse_headers
                  ~max_count
                  ~max_length
                  ~acc:((name, value) :: acc)
                  (StringCursor.create remaining)

  let parse = fun
    ?(max_request_line = 8_192) ?(max_headers = 100) ?(max_header_length = 8_192) input ->
    match parse_request_line ~max_length:max_request_line input with
    | Need_more -> Need_more
    | Error error -> Error error
    | Done { value = (method_, path, version); remaining } -> (
        match parse_headers
          ~max_count:max_headers
          ~max_length:max_header_length
          (StringCursor.create remaining) with
        | Need_more -> Need_more
        | Error error -> Error error
        | Done { value = (headers_list, body); _ } ->
            let method_ = Std.Net.Http.Method.of_string method_ in
            let uri =
              Std.Net.Uri.of_string path
              |> Result.unwrap_or
                ~default:(
                  Std.Net.Uri.of_string "/"
                  |> Result.unwrap
                )
            in
            let version =
              Std.Net.Http.Version.of_string version
              |> Result.unwrap_or ~default:Std.Net.Http.Version.Http11
            in
            let headers = Std.Net.Http.Header.of_list headers_list in
            let request =
              let request = Std.Net.Http.Request.create method_ uri in
              let request = Std.Net.Http.Request.with_version request version in
              let request = Std.Net.Http.Request.with_headers request headers in
              if not (String.equal body "") then
                Std.Net.Http.Request.with_body request body
              else
                request
            in
            Done { value = request; remaining = body }
      )
end

module BorrowedParser = struct
  open Http1.Common

  module Cursor = Std.Iter.Cursor
  module Slice = IO.IoVec.IoSlice

  type t = {
    method_: Slice.t;
    path: Slice.t;
    version: Slice.t;
    headers: (Slice.t * Slice.t) list;
    body: Slice.t;
  }

  type 'a parse_result =
    | Done of {
        value: 'a;
        remaining: Slice.t;
      }
    | Need_more
    | Error of Http1.Common.error

  type request_line = {
    method_: Slice.t;
    path: Slice.t;
    version: Slice.t;
    remaining: Cursor.t;
  }

  type header_line = {
    name: Slice.t;
    value: Slice.t;
    remaining: Cursor.t;
  }

  let is_space = fun c -> c = ' '

  let is_optional_whitespace = fun c -> c = ' ' || c = '\t'

  let trim_leading_ows = fun slice ->
    Cursor.skip_while (Cursor.from_slice slice) is_optional_whitespace
    |> Cursor.remaining

  let skip_line_ending = fun cursor ->
    match Cursor.advance_by cursor 2 with
    | None -> Error InvalidCrlf
    | Some cursor -> Done { value = cursor; remaining = Cursor.remaining cursor }

  let take_header_block_terminator = fun cursor ->
    match Cursor.take_n cursor 2 with
    | Some (prefix, cursor) when Slice.equal_string prefix "\r\n" -> Some cursor
    | _ -> None

  let parse_request_line = fun ?(max_length = 8_192) input ->
    let cursor = Cursor.from_slice input in
    match Cursor.take_until_char cursor '\r' with
    | None -> Need_more
    | Some (line, cursor) ->
        if Slice.length line > max_length then
          Error (RequestLineTooLong { max_length })
        else
          match skip_line_ending cursor with
          | Need_more
          | Error _ as result -> result
          | Done { value = cursor; _ } -> (
              let line_cursor = Cursor.from_slice line in
              match Cursor.take_until_char line_cursor ' ' with
              | None -> Error MissingMethod
              | Some (method_, line_cursor) ->
                  let line_cursor = Cursor.skip_while line_cursor is_space in
                  match Cursor.take_until_char line_cursor ' ' with
                  | None -> Error MissingPath
                  | Some (path, line_cursor) ->
                      let version =
                        Cursor.skip_while line_cursor is_space
                        |> Cursor.remaining
                      in
                      if Slice.starts_with version ~prefix:"HTTP/" then
                        Done {
                          value =
                            {
                              method_;
                              path;
                              version;
                              remaining = cursor;
                            };
                          remaining = Cursor.remaining cursor;
                        }
                      else
                        Error InvalidHttpVersion
            )

  let parse_header_line = fun cursor ->
    match Cursor.take_until_char cursor '\r' with
    | None -> Need_more
    | Some (line, cursor) -> (
        match skip_line_ending cursor with
        | Need_more
        | Error _ as result -> result
        | Done { value = cursor; _ } -> (
            let line_cursor = Cursor.from_slice line in
            match Cursor.take_until_char line_cursor ':' with
            | None -> Error (InvalidHeaderFormat MissingColon)
            | Some (name, line_cursor) -> (
                match Cursor.advance line_cursor with
                | None -> Error (InvalidHeaderFormat MissingValueSeparator)
                | Some line_cursor ->
                    let value =
                      Cursor.skip_while line_cursor is_optional_whitespace
                      |> Cursor.remaining
                    in
                    let name = trim_leading_ows name in
                    Done {
                      value = { name; value; remaining = cursor };
                      remaining = Cursor.remaining cursor;
                    }
              )
          )
      )

  let rec parse_headers = fun
    ?(max_count = 100) ?(max_length = 8_192) ?(acc = []) ?(count = 0) cursor ->
    if count >= max_count then
      Error (TooManyHeaders { max_count })
    else
      match take_header_block_terminator cursor with
      | Some cursor ->
          Done {
            value = (List.rev acc, cursor);
            remaining = Cursor.remaining cursor;
          }
      | None ->
          match parse_header_line cursor with
          | Need_more -> Need_more
          | Error error -> Error error
          | Done { value = { name; value; remaining = next_cursor }; _ } ->
              if Slice.length name + Slice.length value > max_length then
                Error (HeaderTooLong { max_length })
              else
                parse_headers
                  ~max_count
                  ~max_length
                  ~acc:((name, value) :: acc)
                  ~count:(count + 1)
                  next_cursor

  let parse = fun
    ?(max_request_line = 8_192) ?(max_headers = 100) ?(max_header_length = 8_192) input ->
    match parse_request_line ~max_length:max_request_line input with
    | Need_more -> Need_more
    | Error error -> Error error
    | Done { value = {
        method_;
        path;
        version;
        remaining = next_cursor;
      }; _ } ->
        (
            match parse_headers ~max_count:max_headers ~max_length:max_header_length next_cursor with
            | Need_more -> Need_more
            | Error error -> Error error
            | Done { value = (headers, remaining); _ } ->
                let body = Cursor.remaining remaining in
                Done {
                  value =
                    {
                      method_;
                      path;
                      version;
                      headers;
                      body;
                    };
                  remaining = body;
                }
          )
end

let build_request = fun ~method_ ~path ~headers ~body ->
  let head =
    method_
    ^ " "
    ^ path
    ^ " HTTP/1.1\r\n"
    ^ String.concat "" (List.map headers ~fn:(fun (name, value) -> name ^ ": " ^ value ^ "\r\n"))
    ^ "\r\n"
  in
  head ^ body

let build_headers = fun ~count ->
  List.init
    ~count
    ~fn:(fun index -> ("X-Bench-" ^ Int.to_string index, "value-" ^ Int.to_string index))

let build_cookie_header = fun ~count ~value_len ->
  List.init
    ~count
    ~fn:(fun index ->
      "cookie_"
      ^ Int.to_string index
      ^ "="
      ^ String.make ~len:value_len ~char:(Char.from_int_unchecked (97 + (index mod 26))))
  |> String.concat "; "

let small_request =
  build_request
    ~method_:"GET"
    ~path:"/health"
    ~headers:[ ("Host", "example.com"); ("Accept", "*/*"); ]
    ~body:""

let body_1k = String.make ~len:1_024 ~char:'a'

let request_1k =
  build_request
    ~method_:"POST"
    ~path:"/v1/data"
    ~headers:[
      ("Host", "example.com");
      ("Content-Type", "application/json");
      ("Content-Length", Int.to_string (String.length body_1k));
    ]
    ~body:body_1k

let body_100k = String.make ~len:100_000 ~char:'b'

let request_100k =
  build_request
    ~method_:"PUT"
    ~path:"/bulk"
    ~headers:[
      ("Host", "example.com");
      ("Content-Type", "application/octet-stream");
      ("Content-Length", Int.to_string (String.length body_100k));
    ]
    ~body:body_100k

let body_1m = String.make ~len:1_000_000 ~char:'c'

let request_1m =
  build_request
    ~method_:"PATCH"
    ~path:"/archive"
    ~headers:[
      ("Host", "example.com");
      ("Content-Type", "application/octet-stream");
      ("Content-Length", Int.to_string (String.length body_1m));
    ]
    ~body:body_1m

let body_10m = String.make ~len:10_000_000 ~char:'d'

let request_10m =
  build_request
    ~method_:"PATCH"
    ~path:"/archive"
    ~headers:[
      ("Host", "example.com");
      ("Content-Type", "application/octet-stream");
      ("Content-Length", Int.to_string (String.length body_10m));
    ]
    ~body:body_10m

let many_headers_request =
  build_request
    ~method_:"GET"
    ~path:"/headers"
    ~headers:(("Host", "example.com") :: build_headers ~count:80)
    ~body:""

let github_navigation_request =
  let path =
    "/_global-navigation/payloads.json?current_repo_nwo=leostera%2Friot-new"
    ^ "&repository=riot-new"
    ^ "&return_to=https%3A%2F%2Fgithub.com%2Fleostera%2Friot-new%2Fblob%2Fmain%2Fpackages%2Fhttp%2FBENCHMARKS.md"
    ^ "&user_id=leostera"
  in
  let cookie = build_cookie_header ~count:24 ~value_len:96 in
  build_request
    ~method_:"GET"
    ~path
    ~headers:[
      ("Host", "github.com");
      ("Accept", "application/json");
      ("Accept-Language", "en-US,en;q=0.9");
      ("Content-Type", "application/json");
      ("Cookie", cookie);
      ("Github-Verified-Fetch", "true");
      ("Priority", "u=1, i");
      ("Referer", "https://github.com/leostera/riot-new/blob/main/packages/http/BENCHMARKS.md");
      ("Sec-CH-UA", "\"Not-A.Brand\";v=\"24\", \"Chromium\";v=\"146\"");
      ("Sec-CH-UA-Mobile", "?0");
      ("Sec-CH-UA-Platform", "\"macOS\"");
      ("Sec-Fetch-Dest", "empty");
      ("Sec-Fetch-Mode", "cors");
      ("Sec-Fetch-Site", "same-origin");
      (
        "User-Agent",
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/146.0.0.0 Safari/537.36"
      );
      ("X-Fetch-Nonce", "v2:b848b908-2786-d94d-9030-5efcc740d40f");
      ("X-GitHub-Client-Version", "da50e20aef6ab1aa7700fc58a61757b7d7280dfb");
      ("X-Requested-With", "XMLHttpRequest");
    ]
    ~body:""

let small_request_slice =
  IO.IoVec.IoSlice.from_string small_request
  |> Result.unwrap

let request_1k_slice =
  IO.IoVec.IoSlice.from_string request_1k
  |> Result.unwrap

let request_100k_slice =
  IO.IoVec.IoSlice.from_string request_100k
  |> Result.unwrap

let request_1m_slice =
  IO.IoVec.IoSlice.from_string request_1m
  |> Result.unwrap

let request_10m_slice =
  IO.IoVec.IoSlice.from_string request_10m
  |> Result.unwrap

let many_headers_request_slice =
  IO.IoVec.IoSlice.from_string many_headers_request
  |> Result.unwrap

let github_navigation_request_slice =
  IO.IoVec.IoSlice.from_string github_navigation_request
  |> Result.unwrap

let consume_result = fun value remaining ->
  let _ =
    (
      Std.Net.Http.Request.method_ value,
      Std.Net.Http.Request.version value,
      Option.map ~fn:Std.Net.Http.Body.length (Std.Net.Http.Request.body value),
      String.length remaining
    )
  in
  ()

let consume_borrowed_result = fun (value: BorrowedParser.t) remaining ->
  let _ =
    (
      IO.IoVec.IoSlice.length value.method_,
      IO.IoVec.IoSlice.length value.path,
      IO.IoVec.IoSlice.length value.version,
      List.length value.headers,
      IO.IoVec.IoSlice.length value.body,
      IO.IoVec.IoSlice.length remaining
    )
  in
  ()

let bench_parse = fun payload () ->
  match Http1.Request.parse payload with
  | Done { value; remaining } -> consume_result value remaining
  | Need_more -> panic "http1 parser bench expected complete payload"
  | Error error -> panic ("http1 parser bench parse error: " ^ Http1.Common.error_to_string error)

let bench_parse_baseline = fun payload () ->
  match BaselineParser.parse payload with
  | Done { value; remaining } -> consume_result value remaining
  | Need_more -> panic "http1 baseline parser bench expected complete payload"
  | Error error ->
      panic ("http1 baseline parser bench parse error: " ^ Http1.Common.error_to_string error)

let bench_parse_slice = fun payload () ->
  match Http1.Request.parse_slice payload with
  | Done { value; remaining } -> consume_result value remaining
  | Need_more -> panic "http1 slice parser bench expected complete payload"
  | Error error ->
      panic ("http1 slice parser bench parse error: " ^ Http1.Common.error_to_string error)

let bench_parse_borrowed = fun payload () ->
  match BorrowedParser.parse payload with
  | BorrowedParser.Done { value; remaining } -> consume_borrowed_result value remaining
  | BorrowedParser.Need_more -> panic "http1 borrowed slice parser bench expected complete payload"
  | BorrowedParser.Error error ->
      panic ("http1 borrowed slice parser bench parse error: " ^ Http1.Common.error_to_string error)

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 200; warmup = 20 }
      "http1 parser in-memory baseline string: small request"
      (bench_parse_baseline small_request);
    with_config
      ~config:{ iterations = 150; warmup = 15 }
      "http1 parser in-memory baseline string: 1 KiB body"
      (bench_parse_baseline request_1k);
    with_config
      ~config:{ iterations = 60; warmup = 6 }
      "http1 parser in-memory baseline string: 100 KiB body"
      (bench_parse_baseline request_100k);
    with_config
      ~config:{ iterations = 15; warmup = 3 }
      "http1 parser in-memory baseline string: 1 MiB body"
      (bench_parse_baseline request_1m);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "http1 parser in-memory baseline string: 10 MiB body"
      (bench_parse_baseline request_10m);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory baseline string: many headers"
      (bench_parse_baseline many_headers_request);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory baseline string: github navigation request"
      (bench_parse_baseline github_navigation_request);
    with_config
      ~config:{ iterations = 200; warmup = 20 }
      "http1 parser in-memory: small request"
      (bench_parse small_request);
    with_config
      ~config:{ iterations = 150; warmup = 15 }
      "http1 parser in-memory: 1 KiB body"
      (bench_parse request_1k);
    with_config
      ~config:{ iterations = 60; warmup = 6 }
      "http1 parser in-memory: 100 KiB body"
      (bench_parse request_100k);
    with_config
      ~config:{ iterations = 15; warmup = 3 }
      "http1 parser in-memory: 1 MiB body"
      (bench_parse request_1m);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "http1 parser in-memory: 10 MiB body"
      (bench_parse request_10m);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory: many headers"
      (bench_parse many_headers_request);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory: github navigation request"
      (bench_parse github_navigation_request);
    with_config
      ~config:{ iterations = 200; warmup = 20 }
      "http1 parser in-memory slice: small request"
      (bench_parse_slice small_request_slice);
    with_config
      ~config:{ iterations = 150; warmup = 15 }
      "http1 parser in-memory slice: 1 KiB body"
      (bench_parse_slice request_1k_slice);
    with_config
      ~config:{ iterations = 60; warmup = 6 }
      "http1 parser in-memory slice: 100 KiB body"
      (bench_parse_slice request_100k_slice);
    with_config
      ~config:{ iterations = 15; warmup = 3 }
      "http1 parser in-memory slice: 1 MiB body"
      (bench_parse_slice request_1m_slice);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "http1 parser in-memory slice: 10 MiB body"
      (bench_parse_slice request_10m_slice);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory slice: many headers"
      (bench_parse_slice many_headers_request_slice);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory slice: github navigation request"
      (bench_parse_slice github_navigation_request_slice);
    with_config
      ~config:{ iterations = 200; warmup = 20 }
      "http1 parser in-memory borrowed slice: small request"
      (bench_parse_borrowed small_request_slice);
    with_config
      ~config:{ iterations = 150; warmup = 15 }
      "http1 parser in-memory borrowed slice: 1 KiB body"
      (bench_parse_borrowed request_1k_slice);
    with_config
      ~config:{ iterations = 60; warmup = 6 }
      "http1 parser in-memory borrowed slice: 100 KiB body"
      (bench_parse_borrowed request_100k_slice);
    with_config
      ~config:{ iterations = 15; warmup = 3 }
      "http1 parser in-memory borrowed slice: 1 MiB body"
      (bench_parse_borrowed request_1m_slice);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "http1 parser in-memory borrowed slice: 10 MiB body"
      (bench_parse_borrowed request_10m_slice);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory borrowed slice: many headers"
      (bench_parse_borrowed many_headers_request_slice);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory borrowed slice: github navigation request"
      (bench_parse_borrowed github_navigation_request_slice);
  ]

let main ~args = Bench.Cli.main ~name:"http1_parser_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
