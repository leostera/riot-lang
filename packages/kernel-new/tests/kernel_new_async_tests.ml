open Std
module Test = Std.Test
module Kernel = Kernel_new

let ( let* ) = Result.and_then

let lift = function
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string error)

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let with_pipe = fun fn ->
  let* pipe = lift (Kernel.Fs.File.pipe ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Fs.File.close pipe.read_end in
      let _ = Kernel.Fs.File.close pipe.write_end in
      ())
    (fun () -> fn pipe.read_end pipe.write_end)

let test_poll_reports_pipe_readability = fun _ctx ->
  with_pipe
    (fun read_end write_end ->
      let* poll = lift (Kernel.Async.Poll.make ()) in
      let token = Kernel.Async.Token.make 41 in
      let* () = lift
        (Kernel.Async.Poll.register
          poll
          token
          Kernel.Async.Interest.readable
          (Kernel.Fs.File.to_source read_end)) in
      let payload = Kernel.Bytes.of_string "x" in
      let* written = lift (Kernel.Fs.File.write write_end payload) in
      if written != 1 then
        Error "expected pipe write to write one byte"
      else
        let* events = lift (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
        let found =
          List.exists
            (fun event ->
              Kernel.Async.Event.is_readable event
              && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
            events
        in
        if found then
          Ok ()
        else
          Error "expected poll to report readability for pipe source")

let tests = [ Test.case "Async poll reports pipe readability" test_poll_reports_pipe_readability; ]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_async_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
