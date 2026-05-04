open Std
open Std.Data

type fixture = {
  name: string;
  payload: string;
  slice: IO.IoVec.IoSlice.t;
}

let make_numeric_array = fun count ->
  "[" ^ (
    List.init ~count ~fn:Int.to_string
    |> String.concat ","
  ) ^ "]"

let make_string_payload = fun size -> {|{"message":"|} ^ String.make ~len:size ~char:'a' ^ "\"}"

let make_fixture = fun name payload ->
  let slice =
    IO.IoVec.IoSlice.from_string payload
    |> Result.expect ~msg:"failed to create io slice"
  in
  { name; payload; slice }

let fixtures = [
  make_fixture "small object" {|{"ok":true,"id":42,"name":"riot"}|};
  make_fixture "1 KiB numeric array" (make_numeric_array 256);
  make_fixture "100 KiB numeric array" (make_numeric_array 20_000);
  make_fixture "1 MiB numeric array" (make_numeric_array 175_000);
  make_fixture "1 MiB string payload" (make_string_payload (1_024 * 1_024));
]

let bench_json = fun payload ->
  match Json.from_string payload with
  | Ok _ -> ()
  | Error error -> panic ("Json.from_string failed during benchmark: " ^ Json.error_to_string error)

let bench_json_stream_string = fun payload ->
  match JsonStream.from_string payload with
  | Ok _ -> ()
  | Error error ->
      panic ("JsonStream.from_string failed during benchmark: " ^ JsonStream.error_to_string error)

let bench_json_stream_slice = fun slice ->
  match JsonStream.from_slice slice with
  | Ok _ -> ()
  | Error error ->
      panic ("JsonStream.from_slice failed during benchmark: " ^ JsonStream.error_to_string error)

let config_for = fun fixture ->
  match fixture.name with
  | "small object" -> { Bench.iterations = 200; warmup = 20 }
  | "1 KiB numeric array" -> { Bench.iterations = 100; warmup = 10 }
  | "100 KiB numeric array" -> { Bench.iterations = 25; warmup = 5 }
  | "1 MiB numeric array" -> { iterations = 10; warmup = 2 }
  | "1 MiB string payload" -> { iterations = 10; warmup = 2 }
  | _ -> { iterations = 25; warmup = 5 }

let benchmarks =
  List.map
    fixtures
    ~fn:(fun fixture ->
      Bench.compare_with_config
        ~config:(config_for fixture)
        ("json parse: " ^ fixture.name)
        [
          Bench.make_case "Json.from_string" (fun () -> bench_json fixture.payload);
          Bench.make_case
            "JsonStream.from_string"
            (fun () -> bench_json_stream_string fixture.payload);
          Bench.make_case "JsonStream.from_slice" (fun () -> bench_json_stream_slice fixture.slice);
        ])

let main ~args = Bench.Cli.main ~name:"std_data_json_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
