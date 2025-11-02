open Std

let read_file path =
  match Fs.read (Path.v path) with
  | Ok content -> content
  | Error _ -> failwith (format "Failed to read file: %s" path)

let load_fixtures base_path suffix =
  let fixtures_path = Path.v base_path in
  match Fs.read_dir fixtures_path with
  | Error _ -> []
  | Ok iter ->
      let entries = Std.Iter.MutIterator.to_list iter in
      let fixtures =
        List.filter_map
          (fun path ->
            let name = Path.basename path in
            if String.ends_with ~suffix name then
              let base =
                String.sub name 0 (String.length name - String.length suffix)
              in
              let test_file = format "%s/%s" base_path name in
              let expected_file = format "%s/%s.expected" base_path base in
              match Fs.exists (Path.v expected_file) with
              | Ok true -> Some (base, test_file, expected_file)
              | _ -> None
            else None)
          entries
      in
      List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) fixtures

let test_protofile_parse (name, proto_file, expected_file) =
  Test.case (format "Protofile: %s" name) (fun () ->
      let input = read_file proto_file in
      match Protobuf.ProtofileFormat.parse input with
      | Ok ast -> (
          (* Check if expected file exists and compare *)
          match Fs.exists (Path.v expected_file) with
          | Ok true -> (
              let expected_str = read_file expected_file in
              match Data.Json.of_string expected_str with
              | Ok expected_json ->
                  let actual_json = Protobuf.ProtofileFormat.to_json ast in
                  let actual_str = Data.Json.to_string actual_json in
                  let expected_str_clean = Data.Json.to_string expected_json in
                  if actual_str = expected_str_clean then Ok ()
                  else
                    Error
                      (format "JSON mismatch:\nExpected: %s\nActual: %s"
                         expected_str_clean actual_str)
              | Error _ ->
                  (* If expected file is just "OK", just check it parsed *)
                  Ok ())
          | _ ->
              (* No expected file, just check it parsed *)
              Ok ())
      | Error err -> Error (format "Parse error: %s" err))

let test_debug_parse (name, txt_file, _expected_file) =
  Test.case (format "Debug Format: %s" name) (fun () ->
      let input = read_file txt_file in
      match Protobuf.DebugFormat.parse input with
      | Ok fields ->
          let _ = fields in
          Ok ()
      | Error err -> Error (format "Parse error: %s" err))

let hex_to_bytes hex_string =
  let len = String.length hex_string in
  if len mod 2 <> 0 then Error "Hex string must have even length"
  else
    try
      let bytes = Stdlib.Bytes.create (len / 2) in
      for i = 0 to (len / 2) - 1 do
        let hex_byte = String.sub hex_string (i * 2) 2 in
        let byte = int_of_string ("0x" ^ hex_byte) in
        Stdlib.Bytes.set bytes i (Char.chr byte)
      done;
      Ok bytes
    with _ -> Error "Invalid hex string"

let test_wire_decode (name, bin_file, _expected_file) =
  Test.case (format "Wire Format: %s" name) (fun () ->
      let hex_input = read_file bin_file in
      let input_trimmed = String.trim hex_input in
      match hex_to_bytes input_trimmed with
      | Error e -> Error (format "Hex decode error: %s" e)
      | Ok bytes -> (
          match Protobuf.WireFormat.decode bytes with
          | Ok records ->
              (* For now, just check it decodes *)
              let _ = records in
              Ok ()
          | Error err -> Error (format "Decode error: %s" err)))

let test_wire_roundtrip () =
  Test.case "Wire Format: Round-trip" (fun () ->
      (* Simple round-trip: encode some bytes and decode them back *)
      let test_bytes = Stdlib.Bytes.of_string "\x08\x96\x01\x12\x07testing" in
      match Protobuf.WireFormat.decode test_bytes with
      | Error err -> Error (format "Decode error: %s" err)
      | Ok decoded ->
          let encoded = Protobuf.WireFormat.encode decoded in
          if Stdlib.Bytes.equal test_bytes encoded then Ok ()
          else Error "Round-trip mismatch")

let generate_expected_files () =
  let base_dir = "packages/protobuf/tests/fixtures/protofile" in

  let iter =
    match Fs.read_dir (Path.v base_dir) with
    | Error _ ->
        Format.printf "Error reading directory@.";
        exit 1
    | Ok iter -> iter
  in

  let entries = Iter.MutIterator.to_list iter in
  let proto_files =
    List.filter
      (fun path -> String.ends_with ~suffix:".proto" (Path.to_string path))
      entries
  in

  let count = ref 0 in

  List.iter
    (fun proto_name ->
      let proto_str = Path.to_string proto_name in
      let proto_path = Path.v (base_dir ^ "/" ^ proto_str) in
      let base = String.sub proto_str 0 (String.length proto_str - 6) in
      let expected_file = base_dir ^ "/" ^ base ^ ".expected" in

      match Fs.read proto_path with
      | Error err ->
          Format.printf "Error reading %s: %s@." proto_str (IO.error_message err)
      | Ok content -> (
          match Protobuf.ProtofileFormat.parse content with
          | Error err ->
              Format.printf "Skipping %s (parse error: %s)@." proto_str err
          | Ok ast -> (
              let json = Protobuf.ProtofileFormat.to_json ast in
              let json_str = Data.Json.to_string json in

              match Fs.write json_str (Path.v expected_file) with
              | Ok () ->
                  incr count;
                  if !count mod 10 = 0 then
                    Format.printf "Generated %d files...@." !count
              | Error _ -> Format.printf "Error writing %s@." expected_file)))
    proto_files;

  Format.printf "Done! Generated %d expected files@." !count

let () =
  Miniriot.run
    ~main:(fun ~args ->
      (* Check if we should generate expected files *)
      if List.mem "--generate-expected" args then (
        generate_expected_files ();
        Ok ())
      else
        let protofile_fixtures =
          load_fixtures "packages/protobuf/tests/fixtures/protofile" ".proto"
        in
        let debug_fixtures =
          load_fixtures "packages/protobuf/tests/fixtures/debug" ".txt"
        in
        let wire_fixtures =
          load_fixtures "packages/protobuf/tests/fixtures/wire" ".bin"
        in

        let protofile_tests =
          List.map test_protofile_parse protofile_fixtures
        in
        let debug_tests = List.map test_debug_parse debug_fixtures in
        let wire_tests = List.map test_wire_decode wire_fixtures in
        let roundtrip_tests = [ test_wire_roundtrip () ] in

        let all_tests =
          protofile_tests @ debug_tests @ wire_tests @ roundtrip_tests
        in

        Test.Cli.main ~name:"protobuf" ~tests:all_tests ~args)
    ~args:Env.args ()
