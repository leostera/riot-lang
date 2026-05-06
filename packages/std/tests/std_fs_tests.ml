open Std

module Test = Std.Test

let ( let* ) = fun value fn -> Result.and_then value ~fn

type Message.t +=
  | TempdirResult of (string, string) Result.t

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_fs_write_roundtrips_large_binary_payload = fun _ctx ->
  with_tempdir
    "std_fs_write"
    (fun tempdir ->
      let path = Path.(tempdir / Path.v "payload.bin") in
      let payload =
        String.init ~len:(1_024 * 1_024) ~fn:(fun idx -> Char.from_int_unchecked (idx mod 256))
      in
      let* () =
        Fs.write payload path
        |> Result.map_err ~fn:IO.error_message
      in
      let* actual =
        Fs.read_to_string path
        |> Result.map_err ~fn:IO.error_message
      in
      if String.equal actual payload then
        Ok ()
      else
        Error "expected Fs.write to persist the full payload")

let test_fs_copy_overwrites_existing_destination = fun _ctx ->
  with_tempdir
    "std_fs_copy"
    (fun tempdir ->
      let src = Path.(tempdir / Path.v "source.txt") in
      let dst = Path.(tempdir / Path.v "destination.txt") in
      let* () =
        Fs.write "cloned payload" src
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        Fs.write "stale payload" dst
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        Fs.copy ~src ~dst
        |> Result.map_err ~fn:IO.error_message
      in
      let* actual =
        Fs.read_to_string dst
        |> Result.map_err ~fn:IO.error_message
      in
      if String.equal actual "cloned payload" then
        Ok ()
      else
        Error "expected Fs.copy to copy source contents over the destination")

let test_fs_clone_rejects_existing_destination = fun _ctx ->
  with_tempdir
    "std_fs_clone"
    (fun tempdir ->
      let src = Path.(tempdir / Path.v "source.txt") in
      let dst = Path.(tempdir / Path.v "destination.txt") in
      let* () =
        Fs.write "cloned payload" src
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        Fs.write "stale payload" dst
        |> Result.map_err ~fn:IO.error_message
      in
      let clone_result = Fs.clone ~src ~dst in
      let* actual =
        Fs.read_to_string dst
        |> Result.map_err ~fn:IO.error_message
      in
      match clone_result with
      | Ok () -> Error "expected Fs.clone to reject existing destinations"
      | Error IO.File_exists
      | Error IO.Operation_not_supported ->
          if String.equal actual "stale payload" then
            Ok ()
          else
            Error "expected failed Fs.clone to leave existing destination unchanged"
      | Error err -> Error ("expected clone destination error, got " ^ IO.error_message err))

let test_with_tempdir_retries_collisions_under_concurrency = fun _ctx ->
  let parent = self () in
  let workers = 16 in
  let rec spawn_workers remaining =
    if remaining <= 0 then
      ()
    else
      (
        let _worker =
          spawn
            (fun () ->
              let result =
                with_tempdir
                  "std_fs_concurrent"
                  (fun tempdir ->
                    let marker = Path.(tempdir / Path.v "marker.txt") in
                    let* () =
                      Fs.write "ok" marker
                      |> Result.map_err ~fn:IO.error_message
                    in
                    Ok (Path.to_string tempdir))
              in
              send parent (TempdirResult result);
              Ok ())
        in
        spawn_workers (remaining - 1)
      )
  in
  let rec collect remaining seen =
    if remaining = 0 then
      Ok ()
    else
      let selector msg =
        match msg with
        | TempdirResult result -> Select result
        | _ -> Skip
      in
      match receive ~selector ~timeout:(Time.Duration.from_secs 2) () with
      | Error error -> Error error
      | Ok tempdir ->
          if List.exists (String.equal tempdir) seen then
            Error "expected concurrent Fs.with_tempdir calls to use unique directories"
          else
            collect (remaining - 1) (tempdir :: seen)
  in
  spawn_workers workers;
  collect workers []

let tests = [
  Test.case "Fs.write persists complete payloads" test_fs_write_roundtrips_large_binary_payload;
  Test.case "Fs.copy overwrites existing destinations" test_fs_copy_overwrites_existing_destination;
  Test.case "Fs.clone rejects existing destinations" test_fs_clone_rejects_existing_destination;
  Test.case
    "Fs.with_tempdir retries collisions under concurrency"
    test_with_tempdir_retries_collisions_under_concurrency;
]

let main ~args = Test.Cli.main ~name:"std_fs_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
