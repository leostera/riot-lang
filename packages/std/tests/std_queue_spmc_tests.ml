open Std
module Queue = Collections.Queue.SPMC
module HashSet = Collections.HashSet

type Message.t +=
  | Spmc_consumed of int
  | Spmc_consumer_done of int

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 1) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let collect_until_done = fun ~total ~consumer_count ->
  let seen = HashSet.create () in
  let rec loop remaining_values remaining_consumers =
    if remaining_values = 0 && remaining_consumers = 0 then
      Ok seen
    else
      match
        await ~what:"SPMC consumer activity"
          (
            function
            | Spmc_consumed value -> `select (`Consumed value)
            | Spmc_consumer_done idx -> `select (`Done idx)
            | _ -> `skip
          )
      with
      | Error _ as error -> error
      | Ok (`Consumed value) ->
          ignore (HashSet.insert seen ~value);
          loop (remaining_values - 1) remaining_consumers
      | Ok (`Done _) -> loop remaining_values (remaining_consumers - 1)
  in
  loop total consumer_count

let test_fifo = fun _ctx ->
  let queue = Queue.create () in
  Queue.push queue ~value:1;
  Queue.push queue ~value:2;
  Queue.push queue ~value:3;
  match Queue.pop queue, Queue.pop queue, Queue.pop queue, Queue.pop queue with
  | Some 1, Some 2, Some 3, None -> Ok ()
  | _ -> Error "expected Queue.SPMC.pop to preserve FIFO order"

let test_multiple_consumers_receive_each_value_once = fun _ctx ->
  let queue = Queue.create () in
  let consumer_count = 4 in
  let total = 100 in
  let parent = self () in
  for value = 0 to total - 1 do
    Queue.push queue ~value
  done;
  for consumer = 0 to consumer_count - 1 do
    ignore
      (
        spawn
          (fun () ->
            let rec drain () =
              match Queue.pop queue with
              | None ->
                  send parent (Spmc_consumer_done consumer);
                  Ok ()
              | Some value ->
                  send parent (Spmc_consumed value);
                  drain ()
            in
            drain ())
      )
  done;
  match collect_until_done ~total ~consumer_count with
  | Error _ as error -> error
  | Ok seen ->
      if not (Int.equal (HashSet.length seen) total) then
        Error "expected Queue.SPMC consumers to see each value exactly once"
      else if not (Queue.is_empty queue) then
        Error "expected Queue.SPMC to be empty after every consumer drains it"
      else
        Ok ()

let name = "Queue.SPMC"

let tests = [
  Test.case "Queue.SPMC preserves FIFO order" test_fifo;
  Test.case
    "Queue.SPMC lets multiple consumers drain every value exactly once"
    test_multiple_consumers_receive_each_value_once;
]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
