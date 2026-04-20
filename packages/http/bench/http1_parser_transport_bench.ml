open Std
open Http

module Buffer = IO.Buffer
module IoBuffer = IO.IoBuffer

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

let bench_reader_parse = fun ~chunk_size payload () ->
  let reader = String.to_reader ~chunk_size payload |> IO.buffered ~chunk_size:4_096 () in
  let buf = Buffer.create ~size:(String.length payload) in
  match IO.read_to_end reader ~buf with
  | Error error ->
      panic ("http1 parser transport bench read error: " ^ IO.error_message error)
  | Ok _ -> (
      match Http1.Request.parse (Buffer.contents buf) with
      | Done { value; remaining } ->
          consume_result value remaining
      | Need_more ->
          panic "http1 parser transport bench expected complete payload"
      | Error error ->
          panic ("http1 parser transport bench parse error: " ^ error)
    )

let read_to_iobuffer = fun reader ~read_size ->
  let buffer = IoBuffer.create () |> Result.unwrap in
  let rec loop () =
    let _ = IoBuffer.ensure_free buffer read_size |> Result.unwrap in
    let writable = IoBuffer.writable buffer in
    let bufs = IO.Iovec.from_slices [| writable |] in
    match IO.read_vectored reader bufs with
    | Ok 0 ->
        Ok buffer
    | Ok count ->
        let _ = IoBuffer.commit buffer count |> Result.unwrap in
        loop ()
    | Error _ as error ->
        error
  in
  loop ()

let bench_reader_parse_slice = fun ~chunk_size payload () ->
  let reader = String.to_reader ~chunk_size payload |> IO.buffered ~chunk_size:4_096 () in
  match read_to_iobuffer reader ~read_size:4_096 with
  | Error error ->
      panic ("http1 parser transport slice bench read error: " ^ IO.error_message error)
  | Ok buffer -> (
      match Http1.Request.parse_slice (IO.IoBuffer.readable buffer) with
      | Done { value; remaining } ->
          consume_result value remaining
      | Need_more ->
          panic "http1 parser transport slice bench expected complete payload"
      | Error error ->
          panic ("http1 parser transport slice bench parse error: " ^ error)
    )

let bench_reader_parse_slices = fun ~chunk_size payload () ->
  let reader = String.to_reader ~chunk_size payload |> IO.buffered ~chunk_size:4_096 () in
  match read_to_iobuffer reader ~read_size:4_096 with
  | Error error ->
      panic ("http1 parser transport borrowed slice bench read error: " ^ IO.error_message error)
  | Ok buffer -> (
      match Http1.Request.parse_slices (IO.IoBuffer.readable buffer) with
      | Borrowed_done { value; remaining } ->
          consume_borrowed_result value remaining
      | Borrowed_need_more ->
          panic "http1 parser transport borrowed slice bench expected complete payload"
      | Borrowed_error error ->
          panic ("http1 parser transport borrowed slice bench parse error: " ^ error)
    )

let benchmarks =
  Bench.[
    with_config
      ~config:{ iterations = 200; warmup = 20 }
      "http1 parser reader-fed: small request"
      (bench_reader_parse ~chunk_size:32 small_request);
    with_config
      ~config:{ iterations = 150; warmup = 15 }
      "http1 parser reader-fed: 1 KiB body"
      (bench_reader_parse ~chunk_size:64 request_1k);
    with_config
      ~config:{ iterations = 60; warmup = 6 }
      "http1 parser reader-fed: 100 KiB body"
      (bench_reader_parse ~chunk_size:256 request_100k);
    with_config
      ~config:{ iterations = 15; warmup = 3 }
      "http1 parser reader-fed: 1 MiB body"
      (bench_reader_parse ~chunk_size:1024 request_1m);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "http1 parser reader-fed: 10 MiB body"
      (bench_reader_parse ~chunk_size:4096 request_10m);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser reader-fed: many headers"
      (bench_reader_parse ~chunk_size:64 many_headers_request);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser reader-fed: github navigation request"
      (bench_reader_parse ~chunk_size:128 github_navigation_request);
    with_config
      ~config:{ iterations = 200; warmup = 20 }
      "http1 parser reader-fed slice: small request"
      (bench_reader_parse_slice ~chunk_size:32 small_request);
    with_config
      ~config:{ iterations = 150; warmup = 15 }
      "http1 parser reader-fed slice: 1 KiB body"
      (bench_reader_parse_slice ~chunk_size:64 request_1k);
    with_config
      ~config:{ iterations = 60; warmup = 6 }
      "http1 parser reader-fed slice: 100 KiB body"
      (bench_reader_parse_slice ~chunk_size:256 request_100k);
    with_config
      ~config:{ iterations = 15; warmup = 3 }
      "http1 parser reader-fed slice: 1 MiB body"
      (bench_reader_parse_slice ~chunk_size:1024 request_1m);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "http1 parser reader-fed slice: 10 MiB body"
      (bench_reader_parse_slice ~chunk_size:4096 request_10m);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser reader-fed slice: many headers"
      (bench_reader_parse_slice ~chunk_size:64 many_headers_request);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser reader-fed slice: github navigation request"
      (bench_reader_parse_slice ~chunk_size:128 github_navigation_request);
    with_config
      ~config:{ iterations = 200; warmup = 20 }
      "http1 parser reader-fed borrowed slice: small request"
      (bench_reader_parse_slices ~chunk_size:32 small_request);
    with_config
      ~config:{ iterations = 150; warmup = 15 }
      "http1 parser reader-fed borrowed slice: 1 KiB body"
      (bench_reader_parse_slices ~chunk_size:64 request_1k);
    with_config
      ~config:{ iterations = 60; warmup = 6 }
      "http1 parser reader-fed borrowed slice: 100 KiB body"
      (bench_reader_parse_slices ~chunk_size:256 request_100k);
    with_config
      ~config:{ iterations = 15; warmup = 3 }
      "http1 parser reader-fed borrowed slice: 1 MiB body"
      (bench_reader_parse_slices ~chunk_size:1024 request_1m);
    with_config
      ~config:{ iterations = 5; warmup = 1 }
      "http1 parser reader-fed borrowed slice: 10 MiB body"
      (bench_reader_parse_slices ~chunk_size:4096 request_10m);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser reader-fed borrowed slice: many headers"
      (bench_reader_parse_slices ~chunk_size:64 many_headers_request);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser reader-fed borrowed slice: github navigation request"
      (bench_reader_parse_slices ~chunk_size:128 github_navigation_request);
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"http1_parser_transport_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
