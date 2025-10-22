open Std
open Std.Collections
open Miniriot

let test_concurrent_writes_block () =
  match
    Fs.with_tempdir ~prefix:"concurrent_test" (fun tmpdir ->
        let out1 = Path.(tmpdir / Path.v "out1.txt") in
        let out2 = Path.(tmpdir / Path.v "out2.txt") in
        let out3 = Path.(tmpdir / Path.v "out3.txt") in
        let out4 = Path.(tmpdir / Path.v "out4.txt") in

        let content = String.make 100000 'x' in

        let results = HashMap.create () in

        let writer id out =
          let start = Time.Instant.now () in
          Log.info "Writer %s: starting" id;

          let rec write_many n =
            if n = 0 then ()
            else
              let _ = Fs.write content out in
              write_many (n - 1)
          in
          write_many 20;

          let completed = Time.Instant.now () in
          Log.info "Writer %s: completed" id;
          HashMap.insert results id (start, completed) |> ignore
        in

        let coordinator_pid = self () in

        let _ =
          spawn (fun () ->
              writer "w1" out1;
              send coordinator_pid (Message.Msg "done"))
        in

        let _ =
          spawn (fun () ->
              writer "w2" out2;
              send coordinator_pid (Message.Msg "done"))
        in

        let _ =
          spawn (fun () ->
              writer "w3" out3;
              send coordinator_pid (Message.Msg "done"))
        in

        let _ =
          spawn (fun () ->
              writer "w4" out4;
              send coordinator_pid (Message.Msg "done"))
        in

        let rec wait_for_all n =
          if n = 0 then ()
          else
            let _ = receive_any () in
            wait_for_all (n - 1)
        in
        wait_for_all 4;

        let times = HashMap.to_list results |> List.map snd in
        let earliest =
          List.fold_left
            (fun acc (start, _) ->
              if Time.Instant.compare start acc < 0 then start else acc)
            (fst (List.hd times))
            times
        in

        List.iter
          (fun (id, (start, completed)) ->
            let start_ms =
              Time.Duration.to_millis
                (Time.Instant.duration_since ~earlier:earliest start)
            in
            let completed_ms =
              Time.Duration.to_millis
                (Time.Instant.duration_since ~earlier:earliest completed)
            in
            Log.info "Writer %s: %d-%dms" id start_ms completed_ms)
          (HashMap.to_list results);

        let overlaps (s1, c1) (s2, c2) =
          Time.Instant.compare s1 c2 < 0 && Time.Instant.compare c1 s2 > 0
        in

        let has_overlap =
          match times with
          | [ t1; t2; t3; t4 ] ->
              overlaps t1 t2 || overlaps t1 t3 || overlaps t1 t4
              || overlaps t2 t3 || overlaps t2 t4 || overlaps t3 t4
          | _ -> false
        in

        if has_overlap then
          Ok "Writes executed concurrently (overlapping time spans)"
        else
          Ok
            "Writes executed sequentially (no overlap) - Fs.write doesn't yield")
  with
  | Ok (Ok msg) -> Ok msg
  | Ok (Error e) -> Error e
  | Error _ -> Error "Tempdir creation failed"

let test_concurrent_file_writes_do_yield () =
  match
    Fs.with_tempdir ~prefix:"concurrent_file_test" (fun tmpdir ->
        let out1 = Path.(tmpdir / Path.v "out1.txt") in
        let out2 = Path.(tmpdir / Path.v "out2.txt") in
        let out3 = Path.(tmpdir / Path.v "out3.txt") in
        let out4 = Path.(tmpdir / Path.v "out4.txt") in

        let content = String.make 100000 'x' in

        let results = HashMap.create () in

        let writer id out =
          let start = Time.Instant.now () in
          Log.info "File writer %s: starting" id;

          let file =
            Fs.File.create out |> Result.expect ~msg:"Failed to create file"
          in

          let rec write_many n =
            if n = 0 then ()
            else
              let _ =
                Fs.File.write_all file content
                |> Result.expect ~msg:"Write failed"
              in
              write_many (n - 1)
          in
          write_many 20;

          let _ = Fs.File.close file in

          let completed = Time.Instant.now () in
          Log.info "File writer %s: completed" id;
          HashMap.insert results id (start, completed) |> ignore
        in

        let coordinator_pid = self () in

        let _ =
          spawn (fun () ->
              writer "w1" out1;
              send coordinator_pid (Message.Msg "done"))
        in

        let _ =
          spawn (fun () ->
              writer "w2" out2;
              send coordinator_pid (Message.Msg "done"))
        in

        let _ =
          spawn (fun () ->
              writer "w3" out3;
              send coordinator_pid (Message.Msg "done"))
        in

        let _ =
          spawn (fun () ->
              writer "w4" out4;
              send coordinator_pid (Message.Msg "done"))
        in

        let rec wait_for_all n =
          if n = 0 then ()
          else
            let _ = receive_any () in
            wait_for_all (n - 1)
        in
        wait_for_all 4;

        let times = HashMap.to_list results |> List.map snd in
        let earliest =
          List.fold_left
            (fun acc (start, _) ->
              if Time.Instant.compare start acc < 0 then start else acc)
            (fst (List.hd times))
            times
        in

        List.iter
          (fun (id, (start, completed)) ->
            let start_ms =
              Time.Duration.to_millis
                (Time.Instant.duration_since ~earlier:earliest start)
            in
            let completed_ms =
              Time.Duration.to_millis
                (Time.Instant.duration_since ~earlier:earliest completed)
            in
            Log.info "File writer %s: %d-%dms" id start_ms completed_ms)
          (HashMap.to_list results);

        let overlaps (s1, c1) (s2, c2) =
          Time.Instant.compare s1 c2 < 0 && Time.Instant.compare c1 s2 > 0
        in

        let has_overlap =
          match times with
          | [ t1; t2; t3; t4 ] ->
              overlaps t1 t2 || overlaps t1 t3 || overlaps t1 t4
              || overlaps t2 t3 || overlaps t2 t4 || overlaps t3 t4
          | _ -> false
        in

        if has_overlap then
          Ok "File.write_all executed concurrently (overlapping time spans)"
        else
          Error
            "File.write_all executed sequentially (no overlap) - still not \
             yielding!")
  with
  | Ok (Ok msg) -> Ok msg
  | Ok (Error e) -> Error e
  | Error _ -> Error "Tempdir creation failed"

let () =
  Miniriot.run ~rnd:false @@ fun () ->
  let open Lol in
  let results =
    [
      ( "Concurrent Fs.write (expect sequential)",
        test_concurrent_writes_block () );
      ( "Concurrent File.write_all (expect concurrent)",
        test_concurrent_file_writes_do_yield () );
    ]
  in

  results
  |> List.iter (fun (name, result) ->
      match result with
      | Ok msg -> Log.info "[PASS] %s: %s" name msg
      | Error msg -> Log.error "[FAIL] %s: %s" name msg);

  Process.sleep 0.1
