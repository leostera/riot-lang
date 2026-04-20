open Std

module Slice = IO.IoVec.IoSlice
module Method = Net.Http.Method
module Version = Net.Http.Version
module Cursor = Iter.Cursor
module Array = Collections.Array

let sink = ref 0

let method_tag = function
  | Method.Get -> 1
  | Method.Head -> 2
  | Method.Post -> 3
  | Method.Put -> 4
  | Method.Delete -> 5
  | Method.Connect -> 6
  | Method.Options -> 7
  | Method.Trace -> 8
  | Method.Patch -> 9
  | Method.Extension _ -> 10

let version_tag = function
  | Version.Http09 -> 1
  | Version.Http10 -> 2
  | Version.Http11 -> 3
  | Version.Http2 -> 4
  | Version.Http3 -> 5

let current_method_from_slice = fun value ->
  match Slice.length value with
  | 3 when Slice.equal_string value "GET" -> Method.Get
  | 3 when Slice.equal_string value "PUT" -> Method.Put
  | 4 when Slice.equal_string value "HEAD" -> Method.Head
  | 4 when Slice.equal_string value "POST" -> Method.Post
  | 5 when Slice.equal_string value "PATCH" -> Method.Patch
  | 5 when Slice.equal_string value "TRACE" -> Method.Trace
  | 6 when Slice.equal_string value "DELETE" -> Method.Delete
  | 7 when Slice.equal_string value "CONNECT" -> Method.Connect
  | 7 when Slice.equal_string value "OPTIONS" -> Method.Options
  | _ -> Method.Extension (Slice.to_string value)

let equal_tail = fun value ~at suffix ->
  let suffix_len = String.length suffix in
  if Slice.length value - at != suffix_len then
    false
  else
    let rec loop index =
      if index >= suffix_len then
        true
      else if Slice.get_unchecked value ~at:(at + index) = String.get_unchecked suffix ~at:index then
        loop (index + 1)
      else
        false
    in
    loop 0

let optimized_method_from_slice = fun value ->
  match Slice.length value with
  | 3 -> (
      match Slice.get_unchecked value ~at:0 with
      | 'G' when equal_tail value ~at:1 "ET" -> Method.Get
      | 'P' when equal_tail value ~at:1 "UT" -> Method.Put
      | _ -> Method.Extension (Slice.to_string value)
    )
  | 4 -> (
      match Slice.get_unchecked value ~at:0 with
      | 'H' when equal_tail value ~at:1 "EAD" -> Method.Head
      | 'P' when equal_tail value ~at:1 "OST" -> Method.Post
      | _ -> Method.Extension (Slice.to_string value)
    )
  | 5 -> (
      match Slice.get_unchecked value ~at:0 with
      | 'P' when equal_tail value ~at:1 "ATCH" -> Method.Patch
      | 'T' when equal_tail value ~at:1 "RACE" -> Method.Trace
      | _ -> Method.Extension (Slice.to_string value)
    )
  | 6 ->
      if Slice.get_unchecked value ~at:0 = 'D' && equal_tail value ~at:1 "ELETE" then
        Method.Delete
      else
        Method.Extension (Slice.to_string value)
  | 7 -> (
      match Slice.get_unchecked value ~at:0 with
      | 'C' when equal_tail value ~at:1 "ONNECT" -> Method.Connect
      | 'O' when equal_tail value ~at:1 "PTIONS" -> Method.Options
      | _ -> Method.Extension (Slice.to_string value)
    )
  | _ -> Method.Extension (Slice.to_string value)

let current_version_from_slice = fun value ->
  match Slice.length value with
  | 8 when Slice.equal_string value "HTTP/0.9" -> Ok Version.Http09
  | 8 when Slice.equal_string value "HTTP/1.0" -> Ok Version.Http10
  | 8 when Slice.equal_string value "HTTP/1.1" -> Ok Version.Http11
  | 6 when Slice.equal_string value "HTTP/2" -> Ok Version.Http2
  | 8 when Slice.equal_string value "HTTP/2.0" -> Ok Version.Http2
  | 6 when Slice.equal_string value "HTTP/3" -> Ok Version.Http3
  | 8 when Slice.equal_string value "HTTP/3.0" -> Ok Version.Http3
  | _ -> Error `InvalidVersion

let optimized_version_from_slice = fun value ->
  match Slice.length value with
  | 6 ->
      if equal_tail value ~at:0 "HTTP/2" then
        Ok Version.Http2
      else if equal_tail value ~at:0 "HTTP/3" then
        Ok Version.Http3
      else
        Error `InvalidVersion
  | 8 ->
      if equal_tail value ~at:0 "HTTP/0.9" then
        Ok Version.Http09
      else if equal_tail value ~at:0 "HTTP/1.0" then
        Ok Version.Http10
      else if equal_tail value ~at:0 "HTTP/1.1" then
        Ok Version.Http11
      else if equal_tail value ~at:0 "HTTP/2.0" then
        Ok Version.Http2
      else if equal_tail value ~at:0 "HTTP/3.0" then
        Ok Version.Http3
      else
        Error `InvalidVersion
  | _ ->
      Error `InvalidVersion

let current_take_until_char = fun value needle ->
  match Cursor.take_until_char (Cursor.from_slice value) needle with
  | Some (taken, _) -> Some taken
  | None -> None

let optimized_take_until_char = fun value needle ->
  match Slice.index_char value needle with
  | Some stop -> Some (Slice.sub_unchecked value ~off:0 ~len:stop)
  | None -> None

let build_slice = fun value ->
  match Slice.from_string value with
  | Ok slice -> slice
  | Error error ->
      panic ("std io ioslice bench: failed to build slice: " ^ Kernel.IO.Error.message error)

let standard_methods = [|
  "GET";
  "HEAD";
  "POST";
  "PUT";
  "DELETE";
  "CONNECT";
  "OPTIONS";
  "TRACE";
  "PATCH";
|]

let standard_versions = [|
  "HTTP/0.9";
  "HTTP/1.0";
  "HTTP/1.1";
  "HTTP/2";
  "HTTP/2.0";
  "HTTP/3";
  "HTTP/3.0";
|]

let request_lines = [|
  "GET / HTTP/1.1";
  "GET /health HTTP/1.1";
  "POST /api/v1/items HTTP/1.1";
  "OPTIONS /_global-navigation/payloads.json HTTP/1.1";
  "PATCH /repos/leostera/riot-new/issues/1 HTTP/1.1";
|]

let header_lines = [|
  "Host: github.com";
  "User-Agent: curl/8.0";
  "Accept: application/json";
  "Content-Type: application/json";
  "X-GitHub-Client-Version: da50e20aef6ab1aa7700fc58a61757b7d7280dfb";
|]

let mixed_slices =
  Array.init ~count:100_000
    ~fn:(fun index ->
      build_slice
        (Array.get_unchecked standard_methods ~at:(index mod Array.length standard_methods)))

let mixed_version_slices =
  Array.init ~count:100_000
    ~fn:(fun index ->
      build_slice
        (Array.get_unchecked standard_versions ~at:(index mod Array.length standard_versions)))

let mixed_request_line_slices =
  Array.init ~count:100_000
    ~fn:(fun index ->
      build_slice
        (Array.get_unchecked request_lines ~at:(index mod Array.length request_lines)))

let mixed_header_line_slices =
  Array.init ~count:100_000
    ~fn:(fun index ->
      build_slice
        (Array.get_unchecked header_lines ~at:(index mod Array.length header_lines)))

let run_dispatch = fun dispatch ->
  let acc = ref 0 in
  for index = 0 to Array.length mixed_slices - 1 do
    acc := !acc + method_tag (dispatch (Array.get_unchecked mixed_slices ~at:index))
  done;
  sink := !acc

let bench_current () = run_dispatch current_method_from_slice

let bench_optimized () = run_dispatch optimized_method_from_slice

let run_version_dispatch = fun dispatch ->
  let acc = ref 0 in
  for index = 0 to Array.length mixed_version_slices - 1 do
    match dispatch (Array.get_unchecked mixed_version_slices ~at:index) with
    | Ok version -> acc := !acc + version_tag version
    | Error _ -> acc := !acc - 1
  done;
  sink := !acc

let bench_version_current () = run_version_dispatch current_version_from_slice

let bench_version_optimized () = run_version_dispatch optimized_version_from_slice

let run_scan = fun values needle scan ->
  let acc = ref 0 in
  for index = 0 to Array.length values - 1 do
    match scan (Array.get_unchecked values ~at:index) needle with
    | Some taken -> acc := !acc + Slice.length taken
    | None -> acc := !acc - 1
  done;
  sink := !acc

let bench_request_line_scan_current () = run_scan mixed_request_line_slices ' ' current_take_until_char

let bench_request_line_scan_optimized () =
  run_scan mixed_request_line_slices ' ' optimized_take_until_char

let bench_header_line_scan_current () = run_scan mixed_header_line_slices ':' current_take_until_char

let bench_header_line_scan_optimized () =
  run_scan mixed_header_line_slices ':' optimized_take_until_char

let benchmarks =
  Bench.[
    compare_with_config
      ~config:{ iterations = 100; warmup = 10 }
      "std io ioslice pattern match on slices: mixed http methods x 100000"
      [
        make_case "current from_slice shape" bench_current;
        make_case "optimized first-byte dispatch" bench_optimized;
      ];
    compare_with_config
      ~config:{ iterations = 100; warmup = 10 }
      "std io ioslice pattern match on slices: mixed http versions x 100000"
      [
        make_case "current from_slice shape" bench_version_current;
        make_case "optimized first-byte dispatch" bench_version_optimized;
      ];
    compare_with_config
      ~config:{ iterations = 100; warmup = 10 }
      "std io ioslice delimiter scan: request line to first space x 100000"
      [
        make_case "cursor take_until_char" bench_request_line_scan_current;
        make_case "slice index_char+sub" bench_request_line_scan_optimized;
      ];
    compare_with_config
      ~config:{ iterations = 100; warmup = 10 }
      "std io ioslice delimiter scan: header line to colon x 100000"
      [
        make_case "cursor take_until_char" bench_header_line_scan_current;
        make_case "slice index_char+sub" bench_header_line_scan_optimized;
      ];
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"std_io_ioslice_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
