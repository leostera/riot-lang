open Std

let make_lines = fun ~count ~line_len ~char ->
  List.init ~count ~fn:(fun _ -> String.make ~len:line_len ~char ^ "\n")
  |> String.concat ""

type fixture = {
  name: string;
  payload: string;
  chunk_size: int;
}

let fixtures = [
  {
    name = "1024 x 16B lines";
    payload = make_lines ~count:1_024 ~line_len:16 ~char:'a';
    chunk_size = 64;
  };
  {
    name = "256 x 256B lines";
    payload = make_lines ~count:256 ~line_len:256 ~char:'b';
    chunk_size = 128;
  };
  {
    name = "64 x 1KiB lines";
    payload = make_lines ~count:64 ~line_len:1_024 ~char:'c';
    chunk_size = 256;
  };
]

let consume_string_lines = fun payload ~chunk_size ->
  let reader =
    IO.Reader.from_string payload
    |> IO.BufferedReader.of_reader ~chunk_size
  in
  let total = ref 0 in
  let rec loop () =
    match IO.BufferedReader.read_line reader with
    | Ok "" ->
        !total
    | Ok line ->
        total := !total + String.length line;
        loop ()
    | Error () ->
        panic "std io buffered reader bench: string line read failed"
  in
  loop ()

let consume_slice_lines = fun payload ~chunk_size ~materialize ->
  let reader =
    IO.Reader.from_string payload
    |> IO.BufferedReader.of_reader ~chunk_size
  in
  let total = ref 0 in
  let rec loop () =
    match IO.BufferedReader.read_line_slice reader with
    | Ok None ->
        !total
    | Ok (Some line) ->
        if materialize then
          total := !total + String.length (IO.Iovec.IoSlice.to_string line)
        else
          total := !total + IO.Iovec.IoSlice.length line;
        loop ()
    | Error () ->
        panic "std io buffered reader bench: slice line read failed"
  in
  loop ()

let config_for = fun fixture ->
  match fixture.name with
  | "1024 x 16B lines" ->
      { Bench.iterations = 150; warmup = 15 }
  | "256 x 256B lines" ->
      { Bench.iterations = 80; warmup = 8 }
  | "64 x 1KiB lines" ->
      { Bench.iterations = 40; warmup = 4 }
  | _ ->
      { iterations = 50; warmup = 5 }

let benchmarks =
  List.map fixtures ~fn:(fun fixture ->
    Bench.compare_with_config
      ~config:(config_for fixture)
      ("std io buffered reader lines: " ^ fixture.name)
      [
        Bench.make_case "read_line" (fun () ->
          let _ = consume_string_lines fixture.payload ~chunk_size:fixture.chunk_size in
          ());
        Bench.make_case "read_line_slice" (fun () ->
          let _ = consume_slice_lines fixture.payload ~chunk_size:fixture.chunk_size ~materialize:false in
          ());
        Bench.make_case "read_line_slice |> to_string" (fun () ->
          let _ = consume_slice_lines fixture.payload ~chunk_size:fixture.chunk_size ~materialize:true in
          ());
      ])

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"std_io_buffered_reader_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
