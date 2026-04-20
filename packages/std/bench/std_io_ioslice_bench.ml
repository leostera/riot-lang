open Std

module Slice = IO.IoVec.IoSlice
module Method = Net.Http.Method
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

let mixed_slices =
  Array.init ~count:100_000
    ~fn:(fun index ->
      build_slice
        (Array.get_unchecked standard_methods ~at:(index mod Array.length standard_methods)))

let run_dispatch = fun dispatch ->
  let acc = ref 0 in
  for index = 0 to Array.length mixed_slices - 1 do
    acc := !acc + method_tag (dispatch (Array.get_unchecked mixed_slices ~at:index))
  done;
  sink := !acc

let bench_current () = run_dispatch current_method_from_slice

let bench_optimized () = run_dispatch optimized_method_from_slice

let benchmarks =
  Bench.[
    compare_with_config
      ~config:{ iterations = 100; warmup = 10 }
      "std io ioslice pattern match on slices: mixed http methods x 100000"
      [
        make_case "current from_slice shape" bench_current;
        make_case "optimized first-byte dispatch" bench_optimized;
      ];
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"std_io_ioslice_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
