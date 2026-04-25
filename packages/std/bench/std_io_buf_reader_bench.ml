open Std

let make_lines = fun ~count ~line_len ~char ->
  List.init ~count ~fn:(fun _ -> String.make ~len:line_len ~char ^ "\n") |> String.concat ""

let make_runes = fun ~count ->
  let unit = "A" ^ "\xC3\xA9" ^ "\xF0\x9F\x98\x80" in
  List.init ~count ~fn:(fun _ -> unit) |> String.concat ""

type fixture = {
  name: string;
  payload: string;
  chunk_size: int;
}

let fixtures = [
  {
    name = "1024 x 16B lines";
    payload = make_lines ~count:1_024 ~line_len:16 ~char:'a';
    chunk_size = 64
  };
  {
    name = "256 x 256B lines";
    payload = make_lines ~count:256 ~line_len:256 ~char:'b';
    chunk_size = 512
  };
  {
    name = "64 x 1KiB lines";
    payload = make_lines ~count:64 ~line_len:1_024 ~char:'c';
    chunk_size = 2_048
  };
]

let consume_string_lines = fun payload ~chunk_size ->
  let reader = IO.Reader.from_string payload |> IO.BufReader.from_reader ~size:chunk_size in
  let total = ref 0 in
  let rec loop () =
    match IO.BufReader.read_string reader ~until:'\n' with
    | Ok line ->
        total := !total + String.length line;
        loop ()
    | Error IO.End_of_file ->
        !total
    | Error _ ->
        panic "std io buffered reader bench: string line read failed"
  in
  loop ()

let consume_slice_lines = fun payload ~chunk_size ~materialize ->
  let reader = IO.Reader.from_string payload |> IO.BufReader.from_reader ~size:chunk_size in
  let total = ref 0 in
  let rec loop () =
    match IO.BufReader.read_line reader with
    | Ok line ->
        if materialize then
          total := !total + String.length (IO.IoSlice.to_string line)
        else
          total := !total + IO.IoSlice.length line;
        loop ()
    | Error IO.End_of_file ->
        !total
    | Error _ ->
        panic "std io buffered reader bench: slice line read failed"
  in
  loop ()

let consume_read_slices = fun payload ~chunk_size ->
  let reader = IO.Reader.from_string payload |> IO.BufReader.from_reader ~size:chunk_size in
  let total = ref 0 in
  let rec loop () =
    match IO.BufReader.read_slice reader ~until:'\n' with
    | Ok slice ->
        total := !total + IO.IoSlice.length slice;
        loop ()
    | Error IO.End_of_file ->
        !total
    | Error _ ->
        panic "std io buf reader bench: read_slice failed"
  in
  loop ()

let consume_read_runes = fun payload ~chunk_size ->
  let reader = IO.Reader.from_string payload |> IO.BufReader.from_reader ~size:chunk_size in
  let total = ref 0 in
  let rec loop () =
    match IO.BufReader.read_rune reader with
    | Ok rune ->
        total := !total + Unicode.Rune.to_int rune;
        loop ()
    | Error IO.End_of_file ->
        !total
    | Error _ ->
        panic "std io buf reader bench: read_rune failed"
  in
  loop ()

let consume_read_bytes = fun payload ~chunk_size ->
  let reader = IO.Reader.from_string payload |> IO.BufReader.from_reader ~size:chunk_size in
  let total = ref 0 in
  let rec loop () =
    match IO.BufReader.read_byte reader with
    | Ok byte ->
        total := !total + Char.to_int byte;
        loop ()
    | Error IO.End_of_file ->
        !total
    | Error _ ->
        panic "std io buf reader bench: read_byte failed"
  in
  loop ()

let consume_peek_consume_bytes = fun payload ~chunk_size ->
  let reader = IO.Reader.from_string payload |> IO.BufReader.from_reader ~size:chunk_size in
  let total = ref 0 in
  let rec loop () =
    match IO.BufReader.peek reader ~len:1 with
    | Ok slice ->
        total := !total + Char.to_int (IO.IoSlice.get_unchecked slice ~at:0);
        begin
          match IO.BufReader.consume reader ~len:1 with
          | Ok 1 -> loop ()
          | Ok _ -> panic "std io buf reader bench: consume returned unexpected count"
          | Error _ -> panic "std io buf reader bench: consume failed"
        end
    | Error IO.End_of_file ->
        !total
    | Error _ ->
        panic "std io buf reader bench: peek failed"
  in
  loop ()

let consume_buffered_consume_bytes = fun payload ~chunk_size ->
  let reader = IO.Reader.from_string payload |> IO.BufReader.from_reader ~size:chunk_size in
  let total = ref 0 in
  let rec loop () =
    match IO.BufReader.buffered reader with
    | Ok slice ->
        total := !total + Char.to_int (IO.IoSlice.get_unchecked slice ~at:0);
        begin
          match IO.BufReader.consume reader ~len:1 with
          | Ok 1 -> loop ()
          | Ok _ -> panic "std io buf reader bench: buffered consume returned unexpected count"
          | Error _ -> panic "std io buf reader bench: buffered consume failed"
        end
    | Error IO.End_of_file ->
        !total
    | Error _ ->
        panic "std io buf reader bench: buffered failed"
  in
  loop ()

let config_for = fun fixture ->
  match fixture.name with
  | "1024 x 16B lines" -> { Bench.iterations = 150; warmup = 15 }
  | "256 x 256B lines" -> { Bench.iterations = 80; warmup = 8 }
  | "64 x 1KiB lines" -> { Bench.iterations = 40; warmup = 4 }
  | _ -> { iterations = 50; warmup = 5 }

let benchmarks =
  let line_benchmarks =
    List.map fixtures
      ~fn:(fun fixture ->
        Bench.compare_with_config ~config:(config_for fixture) ("std io buf reader lines: "
        ^ fixture.name)
          [ Bench.make_case "read_line"
              (fun () ->
                let _ = consume_string_lines fixture.payload ~chunk_size:fixture.chunk_size in
                ()); Bench.make_case "read_line (slice)"
              (fun () ->
                let _ = consume_slice_lines
                  fixture.payload
                  ~chunk_size:fixture.chunk_size
                  ~materialize:false in
                ()); Bench.make_case "read_line |> to_string"
              (fun () ->
                let _ = consume_slice_lines
                  fixture.payload
                  ~chunk_size:fixture.chunk_size
                  ~materialize:true in
                ()); Bench.make_case "read_slice"
              (fun () ->
                let _ = consume_read_slices fixture.payload ~chunk_size:fixture.chunk_size in
                ()); ])
  in
  let rune_payload = make_runes ~count:8_192 in
  let rune_chunk_size = 4_096 in
  let scan_payload = String.make ~len:(48 * 1_024) ~char:'x' in
  let scan_chunk_size = 4_096 in
  let rune_benchmarks = [
    Bench.compare_with_config ~config:{ iterations = 60; warmup = 6 } "std io buf reader codepoints: 8192 mixed UTF-8 runes"
      [ Bench.make_case "read_byte"
          (fun () ->
            let _ = consume_read_bytes rune_payload ~chunk_size:rune_chunk_size in
            ()); Bench.make_case "read_rune"
          (fun () ->
            let _ = consume_read_runes rune_payload ~chunk_size:rune_chunk_size in
            ()); ];
    Bench.compare_with_config ~config:{ iterations = 60; warmup = 6 } "std io buf reader scanner: 48 KiB ascii bytes"
      [ Bench.make_case "read_byte"
          (fun () ->
            let _ = consume_read_bytes scan_payload ~chunk_size:scan_chunk_size in
            ()); Bench.make_case "peek(1)+consume(1)"
          (fun () ->
            let _ = consume_peek_consume_bytes scan_payload ~chunk_size:scan_chunk_size in
            ()); Bench.make_case "buffered()+consume(1)"
          (fun () ->
            let _ = consume_buffered_consume_bytes scan_payload ~chunk_size:scan_chunk_size in
            ()); ];
  ]
  in
  line_benchmarks @ rune_benchmarks

let main ~args = Bench.Cli.main ~name:"std_io_buf_reader_bench" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
