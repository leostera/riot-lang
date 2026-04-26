open Std

let ( let* ) value fn = Result.and_then value ~fn

let io_error = fun ~op ~path ?related_path detail ->
  Store_error.Io {
    op;
    path;
    related_path;
    detail = Store_error.Fs detail;
  }

let cleanup_file = fun path ->
  match Fs.remove_file path with
  | Ok ()
  | Error _ -> ()

let cleanup_dir = fun path ->
  match Fs.remove_dir_all path with
  | Ok ()
  | Error _ -> ()

let ensure_dir = fun path ->
  Fs.create_dir_all path
  |> Result.map_err ~fn:(fun detail -> io_error ~op:"create_dir_all" ~path detail)

let ensure_parent = fun path -> ensure_dir (Path.dirname path)

type source_kind =
  | File
  | Directory

let validate_source_path = fun ~kind path ->
  let* exists =
    Fs.exists path
    |> Result.map_err ~fn:(fun detail -> io_error ~op:"exists" ~path detail)
  in
  if not exists then
    Error (Store_error.Invalid_source_path { path; reason = Store_error.Source_missing })
  else
    match kind with
    | Directory ->
        let* is_directory =
          Fs.is_dir path
          |> Result.map_err ~fn:(fun detail -> io_error ~op:"is_dir" ~path detail)
        in
        if is_directory then
          Ok ()
        else
          Error (Store_error.Invalid_source_path { path; reason = Store_error.Source_not_directory })
    | File ->
        let* is_directory =
          Fs.is_dir path
          |> Result.map_err ~fn:(fun detail -> io_error ~op:"is_dir" ~path detail)
        in
        if is_directory then
          Error (Store_error.Invalid_source_path { path; reason = Store_error.Source_not_file })
        else
          Ok ()

let write_temp_file = fun ~temp ~content ->
  let* () = ensure_parent temp in
  match Fs.write content temp with
  | Ok () -> Ok ()
  | Error detail ->
      cleanup_file temp;
      Error (io_error ~op:"write" ~path:temp detail)

let copy_to_temp = fun ~src ~temp ->
  let* () = validate_source_path ~kind:File src in
  let* () = ensure_parent temp in
  match Fs.copy ~src ~dst:temp with
  | Ok () -> Ok ()
  | Error detail ->
      cleanup_file temp;
      Error (io_error ~op:"copy" ~path:temp ~related_path:src detail)

let link_temp_if_absent = fun ~temp ~dst ->
  let result =
    let* () = ensure_parent dst in
    let* already_exists =
      Fs.exists dst
      |> Result.map_err ~fn:(fun detail -> io_error ~op:"exists" ~path:dst detail)
    in
    if already_exists then
      Ok ()
    else
      match Fs.hard_link ~src:temp ~dst with
      | Ok () -> Ok ()
      | Error IO.File_exists -> Ok ()
      | Error detail -> Error (io_error ~op:"hard_link" ~path:dst ~related_path:temp detail)
  in
  cleanup_file temp;
  result

let write_object_if_absent = fun ~temp ~dst ~content ->
  let* () = write_temp_file ~temp ~content in
  link_temp_if_absent ~temp ~dst

let copy_file_if_absent = fun ~source ~temp ~dst ->
  let* () = copy_to_temp ~src:source ~temp in
  link_temp_if_absent ~temp ~dst

let replace_with_temp = fun ~temp ~dst ->
  let result =
    let* () = ensure_parent dst in
    match Fs.rename ~src:temp ~dst with
    | Ok () -> Ok ()
    | Error detail -> Error (io_error ~op:"rename" ~path:dst ~related_path:temp detail)
  in
  match result with
  | Ok () -> Ok ()
  | Error _ as error ->
      cleanup_file temp;
      error

let replace_with_object = fun ~temp ~dst ~content ->
  let* () = write_temp_file ~temp ~content in
  replace_with_temp ~temp ~dst

let replace_with_file = fun ~source ~temp ~dst ->
  let* () = copy_to_temp ~src:source ~temp in
  replace_with_temp ~temp ~dst

let rec copy_tree = fun ~src ~dst ->
  let* () = ensure_dir dst in
  let* entries =
    Fs.read_dir src
    |> Result.map_err ~fn:(fun detail -> io_error ~op:"read_dir" ~path:src detail)
  in
  let rec loop () =
    match Iter.MutIterator.next entries with
    | None -> Ok ()
    | Some entry ->
        let src_entry = Path.(src / entry) in
        let dst_entry = Path.(dst / entry) in
        let* is_directory =
          Fs.is_dir src_entry
          |> Result.map_err ~fn:(fun detail -> io_error ~op:"is_dir" ~path:src_entry detail)
        in
        let* () =
          if is_directory then
            copy_tree ~src:src_entry ~dst:dst_entry
          else
            Fs.copy ~src:src_entry ~dst:dst_entry
            |> Result.map_err
              ~fn:(fun detail ->
                io_error ~op:"copy" ~path:dst_entry ~related_path:src_entry detail)
        in
        loop ()
  in
  loop ()

let commit_dir_if_absent = fun ~source_dir ~staging ~dst ->
  let wrap_destination_error ~op error =
    match error with
    | Store_error.Io { detail; _ } ->
        Error (
          Store_error.Io {
            op;
            path = dst;
            related_path = Some source_dir;
            detail;
          }
        )
    | _ -> Error error
  in
  let* () = validate_source_path ~kind:Directory source_dir in
  let* () =
    ensure_parent dst
    |> Result.or_else ~fn:(wrap_destination_error ~op:"create_dir_all")
  in
  let* already_exists =
    Fs.exists dst
    |> Result.map_err
      ~fn:(fun detail ->
        io_error ~op:"exists" ~path:dst ~related_path:source_dir detail)
  in
  if already_exists then (
    cleanup_dir source_dir;
    Ok ()
  ) else
    match Fs.rename ~src:source_dir ~dst with
    | Ok () -> Ok ()
    | Error _ -> (
        match Fs.exists dst with
        | Ok true ->
            cleanup_dir source_dir;
            Ok ()
        | Ok false -> (
            match copy_tree ~src:source_dir ~dst:staging with
            | Error _ as err ->
                cleanup_dir staging;
                err
            | Ok () -> (
                match Fs.rename ~src:staging ~dst with
                | Ok () ->
                    cleanup_dir source_dir;
                    Ok ()
                | Error detail ->
                    let result =
                      match Fs.exists dst with
                      | Ok true ->
                          cleanup_dir source_dir;
                          Ok ()
                      | Ok false ->
                          Error (io_error ~op:"rename" ~path:dst ~related_path:staging detail)
                      | Error exists_detail ->
                          Error (io_error
                            ~op:"exists"
                            ~path:dst
                            ~related_path:source_dir
                            exists_detail)
                    in
                    cleanup_dir staging;
                    result
              )
          )
        | Error detail -> Error (io_error ~op:"exists" ~path:dst ~related_path:source_dir detail)
      )
