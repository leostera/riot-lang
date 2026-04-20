open Std
open Http

let build_request = fun ~method_ ~path ~headers ~body ->
  let head =
    method_ ^ " " ^ path ^ " HTTP/1.1\r\n"
    ^ String.concat ""
        (List.map headers ~fn:(fun (name, value) -> name ^ ": " ^ value ^ "\r\n"))
    ^ "\r\n"
  in
  head ^ body

let build_headers = fun ~count ->
  List.init ~count ~fn:(fun index -> ("X-Bench-" ^ Int.to_string index, "value-" ^ Int.to_string index))

let build_cookie_header = fun ~count ~value_len ->
  List.init ~count ~fn:(fun index ->
    "cookie_" ^ Int.to_string index ^ "=" ^ String.make ~len:value_len ~char:(Char.chr (97 + (index mod 26))))
  |> String.concat "; "

let small_request =
  build_request
    ~method_:"GET"
    ~path:"/health"
    ~headers:[ ("Host", "example.com"); ("Accept", "*/*") ]
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
      ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/146.0.0.0 Safari/537.36");
      ("X-Fetch-Nonce", "v2:b848b908-2786-d94d-9030-5efcc740d40f");
      ("X-GitHub-Client-Version", "da50e20aef6ab1aa7700fc58a61757b7d7280dfb");
      ("X-Requested-With", "XMLHttpRequest");
    ]
    ~body:""

let small_request_slice = IO.Iovec.IoSlice.from_string small_request |> Result.unwrap

let request_1k_slice = IO.Iovec.IoSlice.from_string request_1k |> Result.unwrap

let request_100k_slice = IO.Iovec.IoSlice.from_string request_100k |> Result.unwrap

let request_1m_slice = IO.Iovec.IoSlice.from_string request_1m |> Result.unwrap

let request_10m_slice = IO.Iovec.IoSlice.from_string request_10m |> Result.unwrap

let many_headers_request_slice = IO.Iovec.IoSlice.from_string many_headers_request |> Result.unwrap

let github_navigation_request_slice = IO.Iovec.IoSlice.from_string github_navigation_request |> Result.unwrap

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

let consume_borrowed_result = fun (value : Http1.Request.request_slices) remaining ->
  let _ =
    (
      IO.Iovec.IoSlice.length value.method_,
      IO.Iovec.IoSlice.length value.path,
      IO.Iovec.IoSlice.length value.version,
      List.length value.headers,
      IO.Iovec.IoSlice.length value.body,
      IO.Iovec.IoSlice.length remaining
    )
  in
  ()

let bench_parse = fun payload () ->
  match Http1.Request.parse payload with
  | Done { value; remaining } ->
      consume_result value remaining
  | Need_more ->
      panic "http1 parser bench expected complete payload"
  | Error error ->
      panic ("http1 parser bench parse error: " ^ error)

let bench_parse_slice = fun payload () ->
  match Http1.Request.parse_slice payload with
  | Done { value; remaining } ->
      consume_result value remaining
  | Need_more ->
      panic "http1 slice parser bench expected complete payload"
  | Error error ->
      panic ("http1 slice parser bench parse error: " ^ error)

let bench_parse_slices = fun payload () ->
  match Http1.Request.parse_slices payload with
  | Borrowed_done { value; remaining } ->
      consume_borrowed_result value remaining
  | Borrowed_need_more ->
      panic "http1 borrowed slice parser bench expected complete payload"
  | Borrowed_error error ->
      panic ("http1 borrowed slice parser bench parse error: " ^ error)

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 200; warmup = 20 } "http1 parser in-memory: small request" (bench_parse small_request);
    with_config ~config:{ iterations = 150; warmup = 15 } "http1 parser in-memory: 1 KiB body" (bench_parse request_1k);
    with_config ~config:{ iterations = 60; warmup = 6 } "http1 parser in-memory: 100 KiB body" (bench_parse request_100k);
    with_config ~config:{ iterations = 15; warmup = 3 } "http1 parser in-memory: 1 MiB body" (bench_parse request_1m);
    with_config ~config:{ iterations = 5; warmup = 1 } "http1 parser in-memory: 10 MiB body" (bench_parse request_10m);
    with_config ~config:{ iterations = 120; warmup = 12 } "http1 parser in-memory: many headers" (bench_parse many_headers_request);
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
      (bench_parse_slices small_request_slice);
    with_config
      ~config:{ iterations = 150; warmup = 15 }
      "http1 parser in-memory borrowed slice: 1 KiB body"
      (bench_parse_slices request_1k_slice);
    with_config
      ~config:{ iterations = 60; warmup = 6 }
      "http1 parser in-memory borrowed slice: 100 KiB body"
      (bench_parse_slices request_100k_slice);
    with_config
      ~config:{ iterations = 15; warmup = 3 }
      "http1 parser in-memory borrowed slice: 1 MiB body"
      (bench_parse_slices request_1m_slice);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "http1 parser in-memory borrowed slice: 10 MiB body"
      (bench_parse_slices request_10m_slice);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory borrowed slice: many headers"
      (bench_parse_slices many_headers_request_slice);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory borrowed slice: github navigation request"
      (bench_parse_slices github_navigation_request_slice);
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"http1_parser_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
