open Global
open IO
open Collections

let ( let* ) = fun result fn -> Result.and_then result ~fn

module Engine = Tar_engine

let protect = fun ~finally f ->
  match f () with
  | value ->
      finally ();
      value
  | exception error ->
      finally ();
      raise error

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
  | Engine_error of Engine.error
  | Invalid_path of string
  | Unsafe_path of string
  | Unsupported_entry_kind of entry_kind
  | Duplicate_entry of Path.t

let entry_kind_to_string = fun __tmp1 ->
  match __tmp1 with
  | File -> "file"
  | Directory -> "directory"
  | Symlink -> "symlink"
  | Hardlink -> "hardlink"
  | Other kind -> kind

let engine_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Engine.Invalid_header msg -> "invalid tar header: " ^ msg
  | Engine.Entry_in_progress -> "tar entry is already being read"
  | Engine.Invalid_state msg -> "invalid tar reader state: " ^ msg
  | Engine.Unexpected_eof -> "unexpected end of tar archive"
  | Engine.Out_of_memory -> "tar reader out of memory"
  | Engine.Unknown_error msg -> msg

let error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Engine_error err -> engine_error_to_string err
  | Invalid_path path -> "invalid archive path '" ^ path ^ "'"
  | Unsafe_path path -> "unsafe archive path '" ^ path ^ "'"
  | Unsupported_entry_kind kind ->
      "unsupported archive entry kind '" ^ entry_kind_to_string kind ^ "'"
  | Duplicate_entry path -> "duplicate archive entry '" ^ Path.to_string path ^ "'"

type read_error =
  | Entries_source_error of IO.error
  | Entries_error of error

type extract_error =
  | Extract_source_error of IO.error
  | Extract_fs_error of Fs.error
  | Extract_error of error

type source = {
  reader: Reader.t;
  buffer: Buffer.t;
}

let source_buffer_size = 32 * 1_024

let make_source = fun reader -> { reader; buffer = Buffer.create ~size:source_buffer_size }

let entry_kind_of_engine = fun __tmp1 ->
  match __tmp1 with
  | Engine.File -> File
  | Engine.Directory -> Directory
  | Engine.Symlink -> Symlink
  | Engine.Hardlink -> Hardlink
  | Engine.Other kind -> Other kind

let path_of_string = fun path_str ->
  match Path.from_string path_str with
  | Ok path -> Ok path
  | Error _ -> Error (Invalid_path path_str)

let entry_of_header = fun (header: Engine.header) ->
  let* path = path_of_string header.path in
  let* link_target =
    match header.link_target with
    | None -> Ok None
    | Some target ->
        path_of_string target
        |> Result.map ~fn:Option.some
  in
  Ok {
    path;
    kind = entry_kind_of_engine header.kind;
    size = header.size;
    mode = Option.map header.mode ~fn:Fs.Permissions.from_mode;
    link_target;
  }

let feed_from_source_entries = fun source tar_reader ->
  Buffer.clear source.buffer;
  match IO.read source.reader ~into:source.buffer with
  | Ok 0 -> Ok 0
  | Ok bytes_read ->
      let () = yield () in
      let bytes = Buffer.to_bytes source.buffer in
      let* consumed =
        Engine.feed_reader tar_reader ~src:bytes ~src_pos:0 ~src_len:bytes_read
        |> Result.map_err ~fn:(fun err -> Entries_error (Engine_error err))
      in
      if consumed = bytes_read then
        Ok bytes_read
      else
        Error (Entries_error (Engine_error (Engine.Unknown_error "partial tar feed")))
  | Error err -> Error (Entries_source_error err)

let feed_from_source_extract = fun source tar_reader ->
  Buffer.clear source.buffer;
  match IO.read source.reader ~into:source.buffer with
  | Ok 0 -> Ok 0
  | Ok bytes_read ->
      let () = yield () in
      let bytes = Buffer.to_bytes source.buffer in
      let* consumed =
        Engine.feed_reader tar_reader ~src:bytes ~src_pos:0 ~src_len:bytes_read
        |> Result.map_err ~fn:(fun err -> Extract_error (Engine_error err))
      in
      if consumed = bytes_read then
        Ok bytes_read
      else
        Error (Extract_error (Engine_error (Engine.Unknown_error "partial tar feed")))
  | Error err -> Error (Extract_source_error err)

let next_entry_entries = fun source tar_reader ->
  let rec loop () =
    match Engine.next_entry tar_reader with
    | Error err -> Error (Entries_error (Engine_error err))
    | Ok Engine.Need_input -> (
        match feed_from_source_entries source tar_reader with
        | Ok 0 -> Error (Entries_error (Engine_error Engine.Unexpected_eof))
        | Ok _ -> loop ()
        | Error err -> Error err
      )
    | Ok next -> Ok next
  in
  loop ()

let next_entry_extract = fun source tar_reader ->
  let rec loop () =
    match Engine.next_entry tar_reader with
    | Error err -> Error (Extract_error (Engine_error err))
    | Ok Engine.Need_input -> (
        match feed_from_source_extract source tar_reader with
        | Ok 0 -> Error (Extract_error (Engine_error Engine.Unexpected_eof))
        | Ok _ -> loop ()
        | Error err -> Error err
      )
    | Ok next -> Ok next
  in
  loop ()

let drain_entry_entries = fun source tar_reader ->
  let rec loop () =
    match Engine.skip_entry tar_reader with
    | Error err -> Error (Entries_error (Engine_error err))
    | Ok Engine.Skipped -> Ok ()
    | Ok Engine.Need_input -> (
        match feed_from_source_entries source tar_reader with
        | Ok 0 -> Error (Entries_error (Engine_error Engine.Unexpected_eof))
        | Ok _ -> loop ()
        | Error err -> Error err
      )
  in
  loop ()

let drain_entry_extract = fun source tar_reader ->
  let rec loop () =
    match Engine.skip_entry tar_reader with
    | Error err -> Error (Extract_error (Engine_error err))
    | Ok Engine.Skipped -> Ok ()
    | Ok Engine.Need_input -> (
        match feed_from_source_extract source tar_reader with
        | Ok 0 -> Error (Extract_error (Engine_error Engine.Unexpected_eof))
        | Ok _ -> loop ()
        | Error err -> Error err
      )
  in
  loop ()

let entries = fun reader ->
  match Engine.create_reader () with
  | Error err -> Error (Entries_error (Engine_error err))
  | Ok tar_reader ->
      protect
        ~finally:(fun () -> Engine.close_reader tar_reader)
        (fun () ->
          let source = make_source reader in
          let rec loop acc =
            match next_entry_entries source tar_reader with
            | Error err -> Error err
            | Ok Engine.End -> Ok (List.reverse acc)
            | Ok (Engine.Entry header) ->
                let* entry =
                  entry_of_header header
                  |> Result.map_err ~fn:(fun err -> Entries_error err)
                in
                let* () = drain_entry_entries source tar_reader in
                loop (entry :: acc)
            | Ok Engine.Need_input -> panic "next_entry_entries must not return Need_input"
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
      List.any (Path.components normalized) ~fn:(fun component -> Path.to_string component = "..")
    then
      Error (Unsafe_path path_str)
    else
      Ok (Some normalized)

let register_target = fun seen path ->
  if HashSet.contains seen ~value:path then
    Error (Duplicate_entry path)
  else
    let _ = HashSet.insert seen ~value:path in
    Ok ()

let should_skip_metadata_entry = fun __tmp1 ->
  match __tmp1 with
  | Other "x"
  | Other "g" -> true
  | _ -> false

let should_skip_entry_path = fun path ->
  let name = Path.basename path in
  String.starts_with ~prefix:"._" name
  || String.equal name ".DS_Store"
  || String.equal name "__MACOSX"

let set_permissions = fun path permissions ->
  match permissions with
  | None -> Ok ()
  | Some perms -> Fs.set_permissions path perms

let fs_error_of_file_error = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Fs.File.InvalidSlice _ -> IO.Invalid_argument
  | Kernel.Fs.File.System error -> IO.from_system_error error

let write_entry_file = fun source tar_reader file ->
  let chunk = Bytes.create ~size:source_buffer_size in
  let rec loop chunk_count =
    match Engine.read_entry_data tar_reader ~dst:chunk ~dst_pos:0 ~dst_len:(Bytes.length chunk) with
    | Error err -> Error (Extract_error (Engine_error err))
    | Ok Engine.End_of_entry -> Ok ()
    | Ok (Engine.Chunk bytes_read) ->
        let () =
          if chunk_count > 0 && Int.rem chunk_count 32 = 0 then
            yield ()
        in
        let data =
          Bytes.sub_unchecked chunk ~offset:0 ~len:bytes_read
          |> Bytes.to_string
        in
        let* () =
          Fs.File.write_all file data
          |> Result.map_err ~fn:(fun err -> Extract_fs_error (fs_error_of_file_error err))
        in
        loop (chunk_count + 1)
    | Ok Engine.Need_input -> (
        match feed_from_source_extract source tar_reader with
        | Ok 0 -> Error (Extract_error (Engine_error Engine.Unexpected_eof))
        | Ok _ -> loop chunk_count
        | Error err -> Error err
      )
  in
  loop 0

let extract = fun reader ~into ->
  match Engine.create_reader () with
  | Error err -> Error (Extract_error (Engine_error err))
  | Ok tar_reader ->
      protect
        ~finally:(fun () -> Engine.close_reader tar_reader)
        (fun () ->
          let source = make_source reader in
          let seen = HashSet.create () in
          let rec loop () =
            match next_entry_extract source tar_reader with
            | Error err -> Error err
            | Ok Engine.End -> Ok ()
            | Ok (Engine.Entry header) ->
                let* entry =
                  entry_of_header header
                  |> Result.map_err ~fn:(fun err -> Extract_error err)
                in
                let* relative_path =
                  safe_relative_path ~kind:entry.kind entry.path
                  |> Result.map_err ~fn:(fun err -> Extract_error err)
                in
                let* () =
                  match (entry.kind, relative_path) with
                  | (kind, _) when should_skip_metadata_entry kind ->
                      drain_entry_extract source tar_reader
                  | (_, Some path) when should_skip_entry_path path ->
                      drain_entry_extract source tar_reader
                  | (Directory, None) ->
                      let* () =
                        Fs.create_dir_all into
                        |> Result.map_err ~fn:(fun err -> Extract_fs_error err)
                      in
                      let* () = drain_entry_extract source tar_reader in
                      set_permissions into entry.mode
                      |> Result.map_err ~fn:(fun err -> Extract_fs_error err)
                  | (Directory, Some relative_path) ->
                      let target = Path.join into relative_path in
                      let* () =
                        register_target seen target
                        |> Result.map_err ~fn:(fun err -> Extract_error err)
                      in
                      let* () =
                        Fs.create_dir_all target
                        |> Result.map_err ~fn:(fun err -> Extract_fs_error err)
                      in
                      let* () = drain_entry_extract source tar_reader in
                      set_permissions target entry.mode
                      |> Result.map_err ~fn:(fun err -> Extract_fs_error err)
                  | (File, Some relative_path) ->
                      let target = Path.join into relative_path in
                      let* () =
                        register_target seen target
                        |> Result.map_err ~fn:(fun err -> Extract_error err)
                      in
                      let* () =
                        match Path.parent target with
                        | None -> Ok ()
                        | Some parent ->
                            Fs.create_dir_all parent
                            |> Result.map_err ~fn:(fun err -> Extract_fs_error err)
                      in
                      begin
                        match Fs.File.create target with
                        | Error err -> Error (Extract_fs_error (fs_error_of_file_error err))
                        | Ok file ->
                            protect
                              ~finally:(fun () ->
                                let _ = Fs.File.close file in
                                ())
                              (fun () ->
                                let* () = write_entry_file source tar_reader file in
                                set_permissions target entry.mode
                                |> Result.map_err ~fn:(fun err -> Extract_fs_error err))
                      end
                  | (File, None)
                  | (Symlink, _)
                  | (Hardlink, _)
                  | (Other _, _) -> Error (Extract_error (Unsupported_entry_kind entry.kind))
                in
                loop ()
            | Ok Engine.Need_input -> panic "next_entry_extract must not return Need_input"
          in
          loop ())

let entries_file = fun archive ->
  match Fs.File.open_read archive with
  | Error err -> Error (Entries_source_error (fs_error_of_file_error err))
  | Ok file ->
      protect
        ~finally:(fun () ->
          let _ = Fs.File.close file in
          ())
        (fun () -> entries (Fs.File.to_reader file))

let extract_file = fun ~archive ~into ->
  match Fs.File.open_read archive with
  | Error err -> Error (Extract_source_error (fs_error_of_file_error err))
  | Ok file ->
      protect
        ~finally:(fun () ->
          let _ = Fs.File.close file in
          ())
        (fun () -> extract (Fs.File.to_reader file) ~into)
