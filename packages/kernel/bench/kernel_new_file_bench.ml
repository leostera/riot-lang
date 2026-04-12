open Std
module Kernel = Kernel

let panic_file = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.of_fs_file error))

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix (fun tempdir -> fn (Kernel.Path.of_string (Path.to_string tempdir))) with
  | Ok value -> value
  | Error _ -> Kernel.SystemError.panic "failed to create temporary directory"

let with_temp_path = fun prefix filename fn ->
  match
    Fs.with_tempdir ~prefix
      (fun tempdir ->
        let path = Kernel.Path.(Path.to_string tempdir / filename) in
        fn path)
  with
  | Ok value -> value
  | Error _ -> Kernel.SystemError.panic "failed to create temporary directory"

let with_file = fun file fn ->
  try
    let value = fn file in
    let _ = Kernel.Fs.File.close file in
    value
  with
  | error ->
      let _ = Kernel.Fs.File.close file in
      raise error

let scalar_payload = Kernel.Bytes.of_string (Kernel.String.make 4_096 'x')

let vectored_payload =
  Kernel.IO.Iovec.of_string_array
    (
      Kernel.Array.init 4
        (fun _ ->
          Kernel.String.make 1_024 'x')
    )

let bench_scalar_write = fun () ->
  with_temp_path "kernel_new_file_bench" "scalar.bin"
    (fun path ->
      match Kernel.Fs.File.open_write path with
      | Kernel.Result.Error error -> panic_file error
      | Kernel.Result.Ok file ->
          with_file file
            (fun file ->
              match Kernel.Fs.File.write file scalar_payload with
              | Kernel.Result.Ok _ -> ()
              | Kernel.Result.Error error -> panic_file error))

let bench_partial_write = fun () ->
  with_temp_path "kernel_new_file_bench" "partial.bin"
    (fun path ->
      match Kernel.Fs.File.open_write path with
      | Kernel.Result.Error error -> panic_file error
      | Kernel.Result.Ok file ->
          with_file file
            (fun file ->
              match Kernel.Fs.File.write file ~pos:512 ~len:2_048 scalar_payload with
              | Kernel.Result.Ok _ -> ()
              | Kernel.Result.Error error -> panic_file error))

let bench_vectored_write = fun () ->
  with_temp_path "kernel_new_file_bench" "vectored.bin"
    (fun path ->
      match Kernel.Fs.File.open_write path with
      | Kernel.Result.Error error -> panic_file error
      | Kernel.Result.Ok file ->
          with_file file
            (fun file ->
              match Kernel.Fs.File.write_vectored file vectored_payload with
              | Kernel.Result.Ok _ -> ()
              | Kernel.Result.Error error -> panic_file error))

let bench_scalar_read = fun () ->
  with_temp_path "kernel_new_file_bench" "read.bin"
    (fun path ->
      let _ =
        match Kernel.Fs.File.open_write path with
        | Kernel.Result.Error error -> panic_file error
        | Kernel.Result.Ok file ->
            with_file file
              (fun file ->
                match Kernel.Fs.File.write file scalar_payload with
                | Kernel.Result.Ok _ -> ()
                | Kernel.Result.Error error -> panic_file error)
      in
      match Kernel.Fs.File.open_read path with
      | Kernel.Result.Error error -> panic_file error
      | Kernel.Result.Ok file ->
          let buffer = Kernel.Bytes.create (Kernel.Bytes.length scalar_payload) in
          with_file file
            (fun file ->
              match Kernel.Fs.File.read file buffer with
              | Kernel.Result.Ok _ -> ()
              | Kernel.Result.Error error -> panic_file error))

let bench_partial_read = fun () ->
  with_temp_path "kernel_new_file_bench" "partial-read.bin"
    (fun path ->
      let _ =
        match Kernel.Fs.File.open_write path with
        | Kernel.Result.Error error -> panic_file error
        | Kernel.Result.Ok file ->
            with_file file
              (fun file ->
                match Kernel.Fs.File.write file scalar_payload with
                | Kernel.Result.Ok _ -> ()
                | Kernel.Result.Error error -> panic_file error)
      in
      match Kernel.Fs.File.open_read path with
      | Kernel.Result.Error error -> panic_file error
      | Kernel.Result.Ok file ->
          let buffer = Kernel.Bytes.create (Kernel.Bytes.length scalar_payload) in
          with_file file
            (fun file ->
              match Kernel.Fs.File.read file ~pos:512 ~len:2_048 buffer with
              | Kernel.Result.Ok _ -> ()
              | Kernel.Result.Error error -> panic_file error))

let bench_vectored_read = fun () ->
  with_temp_path "kernel_new_file_bench" "readv.bin"
    (fun path ->
      let _ =
        match Kernel.Fs.File.open_write path with
        | Kernel.Result.Error error -> panic_file error
        | Kernel.Result.Ok file ->
            with_file file
              (fun file ->
                match Kernel.Fs.File.write file scalar_payload with
                | Kernel.Result.Ok _ -> ()
                | Kernel.Result.Error error -> panic_file error)
      in
      match Kernel.Fs.File.open_read path with
      | Kernel.Result.Error error -> panic_file error
      | Kernel.Result.Ok file ->
          let iov = Kernel.IO.Iovec.create ~count:4 ~size:1_024 () in
          with_file file
            (fun file ->
              match Kernel.Fs.File.read_vectored file iov with
              | Kernel.Result.Ok _ -> ()
              | Kernel.Result.Error error -> panic_file error))

let bench_metadata = fun () ->
  with_temp_path "kernel_new_file_bench" "metadata.bin"
    (fun path ->
      let _ =
        match Kernel.Fs.File.open_write path with
        | Kernel.Result.Error error -> panic_file error
        | Kernel.Result.Ok file ->
            with_file file
              (fun file ->
                match Kernel.Fs.File.write file scalar_payload with
                | Kernel.Result.Ok _ -> ()
                | Kernel.Result.Error error -> panic_file error)
      in
      match Kernel.Fs.File.metadata path with
      | Kernel.Result.Ok _ -> ()
      | Kernel.Result.Error error -> panic_file error)

let bench_read_dir_names = fun () ->
  with_tempdir "kernel_new_file_bench"
    (fun tempdir ->
      let _ =
        match Kernel.Fs.File.create_dir Kernel.Path.(tempdir / "child") ~perm:0o755 with
        | Kernel.Result.Ok () -> ()
        | Kernel.Result.Error error -> panic_file error
      in
      let _ =
        match Kernel.Fs.File.open_write Kernel.Path.(tempdir / "alpha.txt") with
        | Kernel.Result.Error error -> panic_file error
        | Kernel.Result.Ok file ->
            with_file file
              (fun file ->
                match Kernel.Fs.File.write file (Kernel.Bytes.of_string "a") with
                | Kernel.Result.Ok _ -> ()
                | Kernel.Result.Error error -> panic_file error)
      in
      match Kernel.Fs.File.read_dir_names tempdir with
      | Kernel.Result.Ok _ -> ()
      | Kernel.Result.Error error -> panic_file error)

let bench_read_dir_names_large = fun () ->
  with_tempdir "kernel_new_file_bench"
    (fun tempdir ->
      let rec create_many index =
        if index = 128 then
          ()
        else
          let name = format Format.[ str "entry-"; int index; str ".txt" ] in
          match Kernel.Fs.File.open_write Kernel.Path.(tempdir / name) with
          | Kernel.Result.Error error -> panic_file error
          | Kernel.Result.Ok file ->
              let _ =
                with_file file
                  (fun file ->
                    match Kernel.Fs.File.write file (Kernel.Bytes.of_string "x") with
                    | Kernel.Result.Ok _ -> ()
                    | Kernel.Result.Error error -> panic_file error)
              in
              create_many (index + 1)
      in
      create_many 0;
      match Kernel.Fs.File.read_dir_names tempdir with
      | Kernel.Result.Ok _ -> ()
      | Kernel.Result.Error error -> panic_file error)

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 20; warmup = 5 } "file scalar write: 4KiB" bench_scalar_write;
    with_config ~config:{ iterations = 20; warmup = 5 } "file partial write: 2KiB@512" bench_partial_write;
    with_config ~config:{ iterations = 20; warmup = 5 } "file vectored write: 4 x 1KiB" bench_vectored_write;
    with_config ~config:{ iterations = 20; warmup = 5 } "file scalar read: 4KiB" bench_scalar_read;
    with_config ~config:{ iterations = 20; warmup = 5 } "file partial read: 2KiB@512" bench_partial_read;
    with_config ~config:{ iterations = 20; warmup = 5 } "file vectored read: 4 x 1KiB" bench_vectored_read;
    with_config ~config:{ iterations = 20; warmup = 5 } "file metadata: 4KiB" bench_metadata;
    with_config ~config:{ iterations = 20; warmup = 5 } "file read_dir_names: 2 entries" bench_read_dir_names;
    with_config ~config:{ iterations = 15; warmup = 3 } "file read_dir_names: 128 entries" bench_read_dir_names_large;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_file_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
