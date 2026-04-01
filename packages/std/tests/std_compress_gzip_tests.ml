open Std
open Std.Compress

let gzip_hello =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\xff\xcbH\xcd\xc9\xc9WH+\xca\xcfUH\xaf\xca,\xe0\x02\x00x-\xbdx\x10\x00\x00\x00"

let with_temp_dir = fun label fn ->
  let temp_root = Path.join (Path.v "/tmp") (Path.v ("riot_" ^ label ^ "_" ^ UUID.to_string (UUID.v4 ()))) in
  match Fs.create_dir_all temp_root with
  | Error err ->
      Error ("failed to create temp dir: " ^ Kernel.IO.error_message err)
  | Ok () ->
      Kernel.Fun.protect
        ~finally:(fun () -> ignore (Fs.remove_dir_all temp_root))
        (fun () -> fn temp_root)

let test_decompress_string = fun () ->
  match Gzip.decompress_string gzip_hello with
  | Ok "hello from gzip\n" ->
      Ok ()
  | Ok text ->
      Error ("unexpected decompressed string: " ^ text)
  | Error _ ->
      Error "failed to decompress known gzip payload"

let test_decompress_file = fun () ->
  with_temp_dir "gzip_file"
    (fun dir ->
      let src = Path.join dir (Path.v "payload.txt.gz") in
      let dst = Path.join dir (Path.v "payload.txt") in
      match Fs.write gzip_hello src with
      | Error err ->
          Error ("failed to write gzip fixture: " ^ Kernel.IO.error_message err)
      | Ok () -> (
          match Gzip.decompress_file ~src ~dst with
          | Error _ ->
              Error "failed to decompress gzip file"
          | Ok () -> (
              match Fs.read_to_string dst with
              | Ok "hello from gzip\n" ->
                  Ok ()
              | Ok text ->
                  Error ("unexpected decompressed file contents: " ^ text)
              | Error err ->
                  Error ("failed to read decompressed file: " ^ Kernel.IO.error_message err)
            )
        ))

let tests =
  Test.[
    case "gzip decompress string" test_decompress_string;
    case "gzip decompress file" test_decompress_file;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"std_compress_gzip" ~tests ~args) ~args:Env.args ()
