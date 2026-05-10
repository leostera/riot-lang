open Std
open Types

let env_paths = fun ?(path = Std.Path.v ".env") ?env () ->
  match env with
  | None -> [ path ]
  | Some env ->
      if String.is_empty env then
        [ path ]
      else
        [ Std.Path.add_extension path ~ext:env; path ]

let emit_load_result = fun path result ->
  match result with
  | Ok bindings -> Telemetry.emit (Events.Loaded { path; binding_count = List.length bindings })
  | Error error -> Telemetry.emit (Events.LoadFailed { path; reason = error_to_string error })

let read_and_parse = fun path ->
  Telemetry.emit (Events.LoadStarted { path });
  match Fs.read path with
  | Error error ->
      let result = Error (ReadError { path; reason = IO.error_message error }) in
      emit_load_result path result;
      result
  | Ok content ->
      let result = Parser.parse content in
      emit_load_result path result;
      result

let missing_file = fun path -> Error (ReadError { path; reason = "file does not exist" })

let first_path = fun paths ->
  match paths with
  | [] -> Std.Path.v ".env"
  | path :: _ -> path

let skip_missing = fun missing ->
  match missing with
  | SkipMissing -> true
  | FailMissing -> false

let load_paths = fun ~on_missing ~require_any ~on_existing paths ->
  match paths with
  | [] -> Ok []
  | _ ->
      let original_paths = paths in
      let apply_paths =
        match on_existing with
        | OverwriteExisting -> List.rev paths
        | PreserveExisting -> paths
      in
      let rec loop_with_original paths loaded_count acc =
        match paths with
        | [] ->
            if require_any && Int.equal loaded_count 0 then
              let path = first_path original_paths in
              let result = missing_file path in
              emit_load_result path result;
              result
            else
              Ok (
                List.rev acc
                |> List.concat
              )
        | path :: rest ->
            Telemetry.emit (Events.LoadStarted { path });
            match Fs.exists path with
            | Error error ->
                let result = Error (ReadError { path; reason = IO.error_message error }) in
                emit_load_result path result;
                result
            | Ok false ->
                if skip_missing on_missing then (
                  Telemetry.emit (Events.LoadSkipped { path });
                  loop_with_original rest loaded_count acc
                ) else
                  let result = missing_file path in
                  emit_load_result path result;
                result
            | Ok true -> (
                match Fs.read path with
                | Error error ->
                    let result = Error (ReadError { path; reason = IO.error_message error }) in
                    emit_load_result path result;
                    result
                | Ok content -> (
                    match Parser.parse content with
                    | Error error ->
                        let result = Error error in
                        emit_load_result path result;
                        result
                    | Ok bindings ->
                        let applied = Environment.apply_collect ~on_existing bindings in
                        emit_load_result path (Ok bindings);
                        loop_with_original rest (loaded_count + 1) (applied :: acc)
                  )
              )
      in
      loop_with_original apply_paths 0 []

let parse_paths = fun ~on_missing paths ->
  let rec loop paths acc =
    match paths with
    | [] ->
        Ok (
          List.rev acc
          |> List.concat
        )
    | path :: rest ->
        match Fs.exists path with
        | Error error -> Error (ReadError { path; reason = IO.error_message error })
        | Ok false ->
            if skip_missing on_missing then
              loop rest acc
            else
              missing_file path
        | Ok true -> (
            match read_and_parse path with
            | Error error -> Error error
            | Ok bindings -> loop rest (bindings :: acc)
          )
  in
  loop paths []

let load_string = fun ?(on_existing = PreserveExisting) content ->
  Parser.parse content
  |> Result.map ~fn:(Environment.apply_collect ~on_existing)

let parse_files = fun ?(on_missing = SkipMissing) paths -> parse_paths ~on_missing paths

let load_files = fun ?(on_existing = PreserveExisting) ?(on_missing = SkipMissing) paths ->
  load_paths
    ~on_missing
    ~require_any:(not (skip_missing on_missing))
    ~on_existing
    paths

let load = fun ?(path = Std.Path.v ".env") ?env ?(on_existing = PreserveExisting) () ->
  let paths = env_paths ~path ?env () in
  match env with
  | None -> load_paths ~on_missing:FailMissing ~require_any:true ~on_existing paths
  | Some _ -> load_paths ~on_missing:SkipMissing ~require_any:true ~on_existing paths

let load_if_exists = fun ?(path = Std.Path.v ".env") ?env ?(on_existing = PreserveExisting) () ->
  load_paths
    ~on_missing:SkipMissing
    ~require_any:false
    ~on_existing
    (env_paths ~path ?env ())
