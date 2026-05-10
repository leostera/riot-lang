open Std

module Test = Std.Test
module BuildLock = Riot_build.BuildLock
module ResultSyntax = Std.Result.Syntax

type Message.t +=
  | BuildLockAcquired of Time.Duration.t
  | BuildLockAcquireFailed of string

let make_workspace = fun root -> Riot_model.Workspace.make_realized ~root ~packages:[] ()

let target_dir_root = fun workspace -> workspace.Riot_model.Workspace.target_dir_root

let target = fun triple ->
  Riot_model.Target.from_string triple
  |> Result.expect ~msg:("expected valid target triple: " ^ triple)

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_reentrant_acquire_in_same_process = fun _ctx ->
  with_tempdir
    "riot_build_lock_reentrant"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let host_target = target "aarch64-apple-darwin" in
      BuildLock.acquire
        ~on_waiting:(fun _ -> ())
        ~target_dir_root:(target_dir_root workspace)
        ~profile:"debug"
        ~target:host_target
        (fun () ->
          let parent = self () in
          let _worker =
            spawn
              (fun () ->
                let start = Time.Instant.now () in
                match BuildLock.acquire
                  ~on_waiting:(fun _ -> ())
                  ~target_dir_root:(target_dir_root workspace)
                  ~profile:"debug"
                  ~target:host_target
                  (fun () ->
                    let waited = Time.Instant.elapsed start in
                    send parent (BuildLockAcquired waited);
                    Ok ()) with
                | Ok () -> Ok ()
                | Error _ ->
                    send parent (BuildLockAcquireFailed "Failed to acquire build lock");
                    Ok ())
          in
          let selector msg =
            match msg with
            | BuildLockAcquired waited -> Select (Ok waited)
            | BuildLockAcquireFailed reason -> Select (Error reason)
            | _ -> Skip
          in
          let early_result =
            try Some (receive ~selector ~timeout:(Time.Duration.from_millis 200) ()) with
            | Receive_timeout -> None
          in
          match early_result with
          | Some (Ok waited) ->
              if Time.Duration.to_millis waited < 200 then
                Ok ()
              else
                Error "Reentrant acquire in the same process should not block"
          | Some (Error reason) -> Error reason
          | None -> Error "Reentrant acquire in the same process should complete promptly"))

let test_releases_lock_on_exception = fun _ctx ->
  with_tempdir
    "riot_build_lock_exception"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let host_target = target "aarch64-apple-darwin" in
      let exception Synthetic_failure in
      try
        let _ =
          BuildLock.acquire
            ~on_waiting:(fun _ -> ())
            ~target_dir_root:(target_dir_root workspace)
            ~profile:"debug"
            ~target:host_target
            (fun () -> raise Synthetic_failure)
        in
        Error "Expected build lock callback to raise"
      with
      | Synthetic_failure ->
          match BuildLock.acquire
            ~on_waiting:(fun _ -> ())
            ~target_dir_root:(target_dir_root workspace)
            ~profile:"debug"
            ~target:host_target
            (fun () -> Ok ()) with
          | Ok () -> Ok ()
          | Error _ -> Error "Build lock was not released after exception")

let test_different_targets_do_not_block_each_other = fun _ctx ->
  with_tempdir
    "riot_build_lock_targets"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let host_target = target "aarch64-apple-darwin" in
      let linux_target = target "aarch64-unknown-linux-gnu" in
      BuildLock.acquire
        ~on_waiting:(fun _ -> ())
        ~target_dir_root:(target_dir_root workspace)
        ~profile:"debug"
        ~target:host_target
        (fun () ->
          let parent = self () in
          let _worker =
            spawn
              (fun () ->
                let start = Time.Instant.now () in
                match BuildLock.acquire
                  ~on_waiting:(fun _ -> ())
                  ~target_dir_root:(target_dir_root workspace)
                  ~profile:"debug"
                  ~target:linux_target
                  (fun () ->
                    let waited = Time.Instant.elapsed start in
                    send parent (BuildLockAcquired waited);
                    Ok ()) with
                | Ok () -> Ok ()
                | Error _ ->
                    send
                      parent
                      (BuildLockAcquireFailed "Failed to acquire build lock for second target");
                    Ok ())
          in
          let selector msg =
            match msg with
            | BuildLockAcquired waited -> Select (Ok waited)
            | BuildLockAcquireFailed reason -> Select (Error reason)
            | _ -> Skip
          in
          match receive ~selector ~timeout:(Time.Duration.from_millis 200) () with
          | Ok waited ->
              if Time.Duration.to_millis waited < 200 then
                Ok ()
              else
                Error "Different target locks should not block each other"
          | Error reason -> Error reason))

let test_existing_lanes_lists_sorted_targets = fun _ctx ->
  with_tempdir
    "riot_build_lock_existing_lanes"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let host_target = target "aarch64-apple-darwin" in
      let linux_target = target "aarch64-unknown-linux-gnu" in
      let open ResultSyntax in
      let* () =
        BuildLock.acquire
          ~on_waiting:(fun _ -> ())
          ~target_dir_root:(target_dir_root workspace)
          ~profile:"release"
          ~target:linux_target
          (fun () -> Ok ())
      in
      let* () =
        BuildLock.acquire
          ~on_waiting:(fun _ -> ())
          ~target_dir_root:(target_dir_root workspace)
          ~profile:"debug"
          ~target:host_target
          (fun () -> Ok ())
      in
      let cache_dir = Path.(target_dir_root workspace / Path.v "cache") in
      let junk_dir = Path.(target_dir_root workspace / Path.v "debug" / Path.v "not-a-target") in
      let _ = Fs.create_dir_all cache_dir in
      let _ = Fs.create_dir_all junk_dir in
      let actual =
        BuildLock.existing_lanes ~target_dir_root:(target_dir_root workspace)
        |> List.map
          ~fn:(fun (lane: BuildLock.lane) ->
            lane.profile ^ ":" ^ Riot_model.Target.to_string lane.target)
      in
      let expected = [ "debug:aarch64-apple-darwin"; "release:aarch64-unknown-linux-gnu" ] in
      if actual = expected then
        Ok ()
      else
        Error ("Expected existing_lanes to return sorted profile/target lanes, got "
        ^ String.concat ", " actual))

let test_acquire_existing_lanes_succeeds_when_no_lanes_exist = fun _ctx ->
  with_tempdir
    "riot_build_lock_empty_lanes"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      BuildLock.acquire_existing_lanes
        ~on_waiting:(fun _ -> ())
        ~target_dir_root:(target_dir_root workspace)
        (fun () -> Ok ()))

let tests =
  Test.[
    case "build lock: reentrant acquire in same process" test_reentrant_acquire_in_same_process;
    case "build lock: releases on exception" test_releases_lock_on_exception;
    case
      "build lock: different targets do not block each other"
      test_different_targets_do_not_block_each_other;
    case "build lock: existing lanes list sorted targets" test_existing_lanes_lists_sorted_targets;
    case
      "build lock: acquire existing lanes succeeds when none exist"
      test_acquire_existing_lanes_succeeds_when_no_lanes_exist;
  ]

let name = "Riot CLI Build Lock Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
