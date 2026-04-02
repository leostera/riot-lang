let collect top_level_dirs =
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | Ok path :: rest -> collect (path :: acc) rest
    | Error _ as err :: _ -> err
  in
  collect [] top_level_dirs

let materialize registry root archive_path =
  match registry with
  | `Filesystem -> (
      let* () = reset_materialized_root root in
      match archive_path with
      | None -> Error "missing archive"
      | Some archive_path -> Ok archive_path
    )
  | `In_memory -> Ok archive_path
