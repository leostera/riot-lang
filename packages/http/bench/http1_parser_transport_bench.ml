open Std
open Http

module Buffer = IO.Buffer

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

let bench_reader_parse = fun ~chunk_size payload () ->
  let reader = String.to_reader ~chunk_size payload |> IO.buffered ~chunk_size:4_096 () in
  let buf = Buffer.create ~size:(String.length payload) in
  match IO.read_to_end reader ~buf with
  | Error error ->
      panic ("http1 parser transport bench read error: " ^ IO.error_message error)
  | Ok _ -> (
      match Http1.Request.parse (Buffer.contents buf) with
      | Done { value; remaining } ->
          let _ =
            (Std.Net.Http.Request.method_ value, Std.Net.Http.Request.version value, String.length remaining)
          in
          ()
      | Need_more ->
          panic "http1 parser transport bench expected complete payload"
      | Error error ->
          panic ("http1 parser transport bench parse error: " ^ error)
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
      ~config:{ iterations = 120; warmup = 12 }
      "http1 parser reader-fed: many headers"
      (bench_reader_parse ~chunk_size:64 many_headers_request);
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"http1_parser_transport_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
