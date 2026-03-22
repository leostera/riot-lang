open Std

module Test = Std.Test
module BuildLock = Tusk_cli.Local_session.BuildLock

type Message.t +=
  | BuildLockAcquired of Time.Duration.t
  | BuildLockAcquireFailed of string

let test_waits_for_existing_lock () =
  match
    Fs.with_tempdir ~prefix:"tusk_build_lock" (fun tmpdir ->
        match BuildLock.acquire tmpdir with
        | Error _ -> Error "Failed to acquire initial build lock"
        | Ok first_lock ->
            let parent = self () in
            let _worker =
              spawn (fun () ->
                  let start = Time.Instant.now () in
                  match BuildLock.acquire tmpdir with
                  | Ok second_lock ->
                      let waited = Time.Instant.elapsed start in
                      BuildLock.release second_lock;
                      send parent (BuildLockAcquired waited);
                      Ok ()
                  | Error _ ->
                      send parent (BuildLockAcquireFailed "Failed to acquire build lock");
                      Ok ())
            in
            let selector msg =
              match msg with
              | BuildLockAcquired waited -> `select (Ok waited)
              | BuildLockAcquireFailed reason -> `select (Error reason)
              | _ -> `skip
            in
            let early_result =
              try Some (receive ~selector ~timeout:(Time.Duration.from_millis 200) ())
              with Receive_timeout -> None
            in
            match early_result with
            | Some (Ok _) ->
                BuildLock.release first_lock;
                Error "Second lock acquisition finished before the first lock was released"
            | Some (Error reason) ->
                BuildLock.release first_lock;
                Error reason
            | None ->
                BuildLock.release first_lock;
                match receive ~selector ~timeout:(Time.Duration.from_secs 2) () with
                | Ok waited ->
                    if Time.Duration.to_millis waited < 200
                    then Error "Second lock acquisition did not wait for release"
                    else Ok ()
                | Error reason -> Error reason)
  with
  | Ok result -> result
  | Error _ -> Error "Tempdir creation failed"

let tests =
  Test.
    [
      case "build lock: waits for existing holder" test_waits_for_existing_lock;
    ]

let name = "Tusk CLI Build Lock Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
