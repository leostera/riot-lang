open Actors
open Actors.Exception
module Result = Std.Result
module Test = Std.Test
module Int = Kernel.Int
module String = Kernel.String

type Message.t +=
  | Pinned_ping
  | Pinned_stop
  | Pinned_scheduler of int
  | Pinned_error of string

let pinned_actor = fun ~parent ~expected_scheduler ->
  let report_scheduler () =
    match current_scheduler_id () with
    | None ->
        send parent (Pinned_error "pinned actor ran outside the normal scheduler pool");
        false
    | Some scheduler_id ->
        let observed = Scheduler_id.to_int scheduler_id in
        if Int.equal observed expected_scheduler then
          (
            send parent (Pinned_scheduler observed);
            true
          )
        else
          (
            send
              parent
              (Pinned_error
                 (String.concat
                    ""
                    [
                      "pinned actor moved schedulers: expected ";
                      Int.to_string expected_scheduler;
                      ", got ";
                      Int.to_string observed;
                    ]));
            false
          )
  in
  let selector = function
    | Pinned_ping -> `select `ping
    | Pinned_stop -> `select `stop
    | _ -> `skip
  in
  let rec loop () =
    match receive ~selector ~timeout:5.0 () with
    | `ping ->
        yield ();
        if report_scheduler () then
          loop ()
        else
          Result.Ok ()
    | `stop -> Result.Ok ()
  in
  if report_scheduler () then
    loop ()
  else
    Result.Ok ()

let recv_report = fun () ->
  receive
    ~selector:(
      function
      | Pinned_scheduler observed -> `select (Result.Ok observed)
      | Pinned_error msg -> `select (Result.Error msg)
      | _ -> `skip
    )
    ~timeout:5.0
    ()

let test_spawn_pinned_stays_on_requested_scheduler = fun () ->
  let parent = self () in
  let expected_scheduler = 1 in
  let pinned =
    spawn_pinned ~scheduler:expected_scheduler
      (fun () -> pinned_actor ~parent ~expected_scheduler)
  in
  match recv_report () with
  | Result.Error _ as err -> err
  | Result.Ok first_observed ->
      if Int.equal first_observed expected_scheduler then
        let rec drive rounds_remaining =
          if Int.equal rounds_remaining 0 then
            (
              send pinned Pinned_stop;
              Result.Ok ()
            )
          else
            (
              send pinned Pinned_ping;
              match recv_report () with
              | Result.Error _ as err -> err
              | Result.Ok observed ->
                  if Int.equal observed expected_scheduler then
                    drive (Int.pred rounds_remaining)
                  else
                    Result.Error
                      (String.concat
                         ""
                         [
                           "pinned actor moved off scheduler ";
                           Int.to_string expected_scheduler;
                           ", got ";
                           Int.to_string observed;
                         ])
            )
        in
        drive 8
      else
        Result.Error
          (String.concat
             ""
             [
               "expected initial pinned scheduler ";
               Int.to_string expected_scheduler;
               ", got ";
               Int.to_string first_observed;
             ])

let test_case = fun _ctx ->
  try test_spawn_pinned_stays_on_requested_scheduler () with
  | Receive_timeout -> Result.Error "timed out while testing pinned spawn"
  | exn -> Result.Error (Kernel.Exception.to_string exn)

let () =
  let tests = [
    Test.case "spawn_pinned stays on the requested scheduler" test_case;
  ] in
  let normalize_args = function
    | [] -> [ "spawn_pinned_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"spawn_pinned_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Actors.run ~main ~args:Std.Env.args ~config:(Actors.Config.make ~scheduler_count:2 ()) ()
