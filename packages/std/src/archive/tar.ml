open Global
open IO
open Collections

let ( let* ) = Result.and_then

type entry_kind =
  | File
  | Directory
  | Symlink
  | Hardlink
  | Other of string

type entry = {
  path: Path.t;
  kind: entry_kind;
  size: int64;
  mode: Fs.Permissions.t option;
  link_target: Path.t option;
}

type error =
  | Kernel_error of Kernel.Archive.Tar.error
  | Invalid_path of string
  | Unsafe_path of string
  | Unsupported_entry_kind of entry_kind
  | Duplicate_entry of Path.t

type 'read_err read_error =
  | Entries_source_error of 'read_err
  | Entries_error of error

type 'read_err extract_error =
  | Extract_source_error of 'read_err
  | Extract_fs_error of Fs.error
  | Extract_error of error

type ('src, 'read_err) source = {
  reader: ('src, 'read_err) Reader.t;
  buffer: Bytes.t;
}

let source_buffer_size = 32 * 1_024

let make_source = fun reader -> { reader; buffer = Bytes.create source_buffer_size }

let entry_kind_of_kernel = function
  | Kernel.Archive.Tar.File -> File
  | Kernel.Archive.Tar.Directory -> Directory
  | Kernel.Archive.Tar.Symlink -> Symlink
  | Kernel.Archive.Tar.Hardlink -> Hardlink
  | Kernel.Archive.Tar.Other kind -> Other kind

let path_of_string = fun path_str ->
  match Path.of_string path_str with
  | Ok path -> Ok path
  | Error _ -> Error (Invalid_path path_str)

let entry_of_header = fun (header: Kernel.Archive.Tar.header) ->
  let* path = path_of_string header.path in
  let* link_target =
    match header.link_target with
    | None -> Ok None
    | Some target -> path_of_string target |> Result.map Option.some
  in
  Ok {
    path;
    kind = entry_kind_of_kernel header.kind;
    size = header.size;
    mode = Option.map Fs.Permissions.of_mode header.mode;
    link_target;
  }

let feed_from_source_entries = fun source tar_reader ->
  match IO.read source.reader source.buffer with
  | Ok 0 ->
      Ok 0
  | Ok bytes_read ->
      let () = yield () in
      let* consumed = Kernel.Archive.Tar.feed_reader
        tar_reader
        ~src:source.buffer
        ~src_pos:0
        ~src_len:bytes_read
      |> Result.map_err (fun err -> Entries_error (Kernel_error err)) in
      if consumed = bytes_read then
        Ok bytes_read
      else
        Error (Entries_error (Kernel_error (Kernel.Archive.Tar.Unknown_error "partial tar feed")))
  | Error err ->
      Error (Entries_source_error err)

let feed_from_source_extract = fun source tar_reader ->
  match IO.read source.reader source.buffer with
  | Ok 0 ->
      Ok 0
  | Ok bytes_read ->
      let () = yield () in
      let* consumed = Kernel.Archive.Tar.feed_reader
        tar_reader
        ~src:source.buffer
        ~src_pos:0
        ~src_len:bytes_read
      |> Result.map_err (fun err -> Extract_error (Kernel_error err)) in
      if consumed = bytes_read then
        Ok bytes_read
      else
        Error (Extract_error (Kernel_error (Kernel.Archive.Tar.Unknown_error "partial tar feed")))
  | Error err ->
      Error (Extract_source_error err)

let next_entry_entries = fun source tar_reader ->
  let rec loop () =
    match Kernel.Archive.Tar.next_entry tar_reader with
    | Error err ->
        Error (Entries_error (Kernel_error err))
    | Ok Kernel.Archive.Tar.Need_input -> (
        match feed_from_source_entries source tar_reader with
        | Ok 0 -> Error (Entries_error (Kernel_error Kernel.Archive.Tar.Unexpected_eof))
        | Ok _ -> loop ()
        | Error err -> Error err
      )
    | Ok next ->
        Ok next
  in
  loop ()

let next_entry_extract = fun source tar_reader ->
  let rec loop () =
    match Kernel.Archive.Tar.next_entry tar_reader with
    | Error err ->
        Error (Extract_error (Kernel_error err))
    | Ok Kernel.Archive.Tar.Need_input -> (
        match feed_from_source_extract source tar_reader with
        | Ok 0 -> Error (Extract_error (Kernel_error Kernel.Archive.Tar.Unexpected_eof))
        | Ok _ -> loop ()
        | Error err -> Error err
      )
    | Ok next ->
        Ok next
  in
  loop ()

let drain_entry_entries = fun source tar_reader ->
  let rec loop () =
    match Kernel.Archive.Tar.skip_entry tar_reader with
    | Error err ->
        Error (Entries_error (Kernel_error err))
    | Ok Kernel.Archive.Tar.Skipped ->
        Ok ()
    | Ok Kernel.Archive.Tar.Need_input -> (
        match feed_from_source_entries source tar_reader with
        | Ok 0 -> Error (Entries_error (Kernel_error Kernel.Archive.Tar.Unexpected_eof))
        | Ok _ -> loop ()
        | Error err -> Error err
      )
  in
  loop ()

let drain_entry_extract = fun source tar_reader ->
  let rec loop () =
    match Kernel.Archive.Tar.skip_entry tar_reader with
    | Error err ->
        Error (Extract_error (Kernel_error err))
    | Ok Kernel.Archive.Tar.Skipped ->
        Ok ()
    | Ok Kernel.Archive.Tar.Need_input -> (
        match feed_from_source_extract source tar_reader with
        | Ok 0 -> Error (Extract_error (Kernel_error Kernel.Archive.Tar.Unexpected_eof))
        | Ok _ -> loop ()
        | Error err -> Error err
      )
  in
  loop ()

let entries = fun reader ->
  match Kernel.Archive.Tar.create_reader () with
  | Error err -> Error (Entries_error (Kernel_error err))
  | Ok tar_reader ->
      Kernel.Fun.protect ~finally:(fun () -> Kernel.Archive.Tar.close_reader tar_reader)
        (fun () ->
          let source = make_source reader in
          let rec loop acc =
            match next_entry_entries source tar_reader with
            | Error err ->
                Error err
            | Ok Kernel.Archive.Tar.End ->
                Ok (List.rev acc)
            | Ok (Kernel.Archive.Tar.Entry header) ->
                let* entry = entry_of_header header |> Result.map_err (fun err -> Entries_error err) in
                let* () = drain_entry_entries source tar_reader in
                loop (entry :: acc)
            | Ok Kernel.Archive.Tar.Need_input ->
                panic "next_entry_entries must not return Need_input"
          in
          loop [])

let safe_relative_path = fun ~kind path ->
  if Path.is_absolute path then
    Error (Unsafe_path (Path.to_string path))
  else
    let normalized = Path.normalize path in
    let path_str = Path.to_string normalized in
    if path_str = "." then
      match kind with
      | Directory -> Ok None
      | _ -> Error (Unsafe_path path_str)
    else if path_str = "" then
      Error (Unsafe_path path_str)
    else if
      List.exists (fun component -> Path.to_string component = "..") (Path.components normalized)
    then
      Error (Unsafe_path path_str)
    else
      Ok (Some normalized)

let register_target = fun seen path ->
  if HashSet.contains seen path then
    Error (Duplicate_entry path)
  else
    let _ = HashSet.insert seen path in
    Ok ()

let set_permissions = fun path permissions ->
  match permissions with
  | None -> Ok ()
  | Some perms -> Fs.set_permissions path perms

let write_entry_file = fun source tar_reader file ->
  let chunk = Bytes.create source_buffer_size in
  let rec loop chunk_count =
    match Kernel.Archive.Tar.read_entry_data
      tar_reader
      ~dst:chunk
      ~dst_pos:0
      ~dst_len:(Bytes.length chunk) with
    | Error err ->
        Error (Extract_error (Kernel_error err))
    | Ok Kernel.Archive.Tar.End_of_entry ->
        Ok ()
    | Ok (Kernel.Archive.Tar.Chunk bytes_read) ->
        let () =
          if chunk_count > 0 && Int.rem chunk_count 32 = 0 then
            yield ()
        in
        let data = Bytes.sub_string chunk 0 bytes_read in
        let* () = Fs.File.write_all file data |> Result.map_err (fun err -> Extract_fs_error err) in
        loop (chunk_count + 1)
    | Ok Kernel.Archive.Tar.Need_input -> (
        match feed_from_source_extract source tar_reader with
        | Ok 0 -> Error (Extract_error (Kernel_error Kernel.Archive.Tar.Unexpected_eof))
        | Ok _ -> loop chunk_count
        | Error err -> Error err
      )
  in
  loop 0

let extract = fun reader ~into ->
  match Kernel.Archive.Tar.create_reader () with
  | Error err -> Error (Extract_error (Kernel_error err))
  | Ok tar_reader ->
      Kernel.Fun.protect ~finally:(fun () -> Kernel.Archive.Tar.close_reader tar_reader)
        (fun () ->
          let source = make_source reader in
          let seen = HashSet.create () in
          let rec loop () =
            match next_entry_extract source tar_reader with
            | Error err ->
                Error err
            | Ok Kernel.Archive.Tar.End ->
                Ok ()
            | Ok (Kernel.Archive.Tar.Entry header) ->
                let* entry = entry_of_header header |> Result.map_err (fun err -> Extract_error err) in
                let* relative_path = safe_relative_path ~kind:entry.kind entry.path
                |> Result.map_err (fun err -> Extract_error err) in
                let* () =
                  match entry.kind, relative_path with
                  | Directory, None ->
                      let* () = Fs.create_dir_all into
                      |> Result.map_err (fun err -> Extract_fs_error err) in
                      let* () = drain_entry_extract source tar_reader in
                      set_permissions into entry.mode
                      |> Result.map_err (fun err -> Extract_fs_error err)
                  | Directory, Some relative_path ->
                      let target = Path.join into relative_path in
                      let* () = register_target seen target
                      |> Result.map_err (fun err -> Extract_error err) in
                      let* () = Fs.create_dir_all target
                      |> Result.map_err (fun err -> Extract_fs_error err) in
                      let* () = drain_entry_extract source tar_reader in
                      set_permissions target entry.mode
                      |> Result.map_err (fun err -> Extract_fs_error err)
                  | File, Some relative_path ->
                      let target = Path.join into relative_path in
                      let* () = register_target seen target
                      |> Result.map_err (fun err -> Extract_error err) in
                      let* () =
                        match Path.parent target with
                        | None -> Ok ()
                        | Some parent -> Fs.create_dir_all parent
                        |> Result.map_err (fun err -> Extract_fs_error err)
                      in
                      begin
                        match Fs.File.create target with
                        | Error err -> Error (Extract_fs_error err)
                        | Ok file ->
                            Kernel.Fun.protect ~finally:(fun () -> ignore (Fs.File.close file))
                              (fun () ->
                                let* () = write_entry_file source tar_reader file in
                                set_permissions target entry.mode
                                |> Result.map_err (fun err -> Extract_fs_error err))
                      end
                  | (File, None)
                  | (Symlink, _)
                  | (Hardlink, _)
                  | (Other _, _) ->
                      Error (Extract_error (Unsupported_entry_kind entry.kind))
                in
                loop ()
            | Ok Kernel.Archive.Tar.Need_input ->
                panic "next_entry_extract must not return Need_input"
          in
          loop ())

let entries_file = fun archive ->
  match Fs.File.open_read archive with
  | Error err -> Error (Entries_source_error err)
  | Ok file -> Kernel.Fun.protect
    ~finally:(fun () -> ignore (Fs.File.close file))
    (fun () -> entries (Fs.File.to_reader file))

let extract_file = fun ~archive ~into ->
  match Fs.File.open_read archive with
  | Error err -> Error (Extract_source_error err)
  | Ok file -> Kernel.Fun.protect
    ~finally:(fun () -> ignore (Fs.File.close file))
    (fun () -> extract (Fs.File.to_reader file) ~into)
