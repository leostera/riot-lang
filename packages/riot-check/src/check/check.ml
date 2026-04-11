open Std
module Check_error = Error
module State = State
module Event = Event
module Reporter = Reporter
module Session = Session

let emit = fun ?on_event event ->
  match on_event with
  | Some callback -> callback event
  | None -> ()

let check_all = fun ~workspace ?package_filter ?on_start ?on_result ?on_event paths ->
  let scan_mode = List.is_empty paths in
  let include_dev = not scan_mode in
  match Scope.resolve_targets ~workspace ?package_filter ~include_dev paths with
  | Error _ as err -> err
  | Ok target_files ->
      let summary = ref State.empty_checked_summary in
      let () =
        match on_start with
        | Some callback -> callback (List.length target_files)
        | None -> ()
      in
      let _checked_files =
        Session.check_target_files ~workspace ~scan_mode ~include_dev ?on_event
          ~on_result:(fun checked_file ->
            summary := State.update_checked_summary !summary checked_file;
            match on_result with
            | Some callback -> callback checked_file
            | None -> ())
          target_files
      in
      Ok State.{ target_count = List.length target_files; summary = !summary }

let run = fun ?on_event ~workspace ~paths ~package_filter () ->
  match Session.prepare_workspace workspace with
  | Error _ as err -> err
  | Ok workspace ->
      let () = emit
        ?on_event
        (Event.WorkspacePrepared {
          packages = workspace.packages
          |> List.map (fun (pkg: Riot_model.Package.t) -> (pkg.name, pkg.path))
        }) in
      let on_start target_count = emit ?on_event (Event.Start { target_count }) in
      let on_result checked_file =
        let () = emit ?on_event (Event.File checked_file) in
        Event.diagnostic_events checked_file |> List.iter (emit ?on_event)
      in
      match check_all ~workspace ?package_filter ~on_start ~on_result ?on_event paths with
      | Error _ as err -> err
      | Ok { summary; _ } ->
          let () = emit ?on_event (Event.Summary { summary }) in
          if summary.has_error then
            Error Check_error.TypecheckFailed
          else
            Ok ()
