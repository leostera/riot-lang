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

let many_headers_request =
  build_request
    ~method_:"GET"
    ~path:"/headers"
    ~headers:(("Host", "example.com") :: build_headers ~count:80)
    ~body:""

let small_request_view = IO.StringView.of_string small_request

let request_1k_view = IO.StringView.of_string request_1k

let request_100k_view = IO.StringView.of_string request_100k

let request_1m_view = IO.StringView.of_string request_1m

let many_headers_request_view = IO.StringView.of_string many_headers_request

let consume_result = fun value remaining ->
  let _ =
    (Std.Net.Http.Request.method_ value, Std.Net.Http.Request.version value, String.length remaining)
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

let bench_parse_string_view = fun payload () ->
  match Http1.Request.parse_string_view payload with
  | Done { value; remaining } ->
      consume_result value remaining
  | Need_more ->
      panic "http1 string_view parser bench expected complete payload"
  | Error error ->
      panic ("http1 string_view parser bench parse error: " ^ error)

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 200; warmup = 20 } "http1 parser in-memory: small request" (bench_parse small_request);
    with_config ~config:{ iterations = 150; warmup = 15 } "http1 parser in-memory: 1 KiB body" (bench_parse request_1k);
    with_config ~config:{ iterations = 60; warmup = 6 } "http1 parser in-memory: 100 KiB body" (bench_parse request_100k);
    with_config ~config:{ iterations = 15; warmup = 3 } "http1 parser in-memory: 1 MiB body" (bench_parse request_1m);
    with_config ~config:{ iterations = 120; warmup = 12 } "http1 parser in-memory: many headers" (bench_parse many_headers_request);
    with_config
      ~config:{ iterations = 200; warmup = 20 }
      "http1 parser in-memory string_view: small request"
      (bench_parse_string_view small_request_view);
    with_config
      ~config:{ iterations = 150; warmup = 15 }
      "http1 parser in-memory string_view: 1 KiB body"
      (bench_parse_string_view request_1k_view);
    with_config
      ~config:{ iterations = 60; warmup = 6 }
      "http1 parser in-memory string_view: 100 KiB body"
      (bench_parse_string_view request_100k_view);
    with_config
      ~config:{ iterations = 15; warmup = 3 }
      "http1 parser in-memory string_view: 1 MiB body"
      (bench_parse_string_view request_1m_view);
    with_config
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser in-memory string_view: many headers"
      (bench_parse_string_view many_headers_request_view);
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"http1_parser_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
