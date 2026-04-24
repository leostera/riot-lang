open Std
module Queue = Collections.Queue.MPMC
module HashSet = Collections.HashSet

type Message.t +=
  | Mpmc_value of int
  | Mpmc_consumer_done of int

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 2) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let test_fifo = fun _ctx ->
  let queue = Queue.create () in
  Queue.push queue ~value:1;
  Queue.push queue ~value:2;
  Queue.push queue ~value:3;
  match Queue.pop queue, Queue.pop queue, Queue.pop queue, Queue.pop queue with
  | Some 1, Some 2, Some 3, None -> Ok ()
  | _ -> Error "expected Queue.MPMC.pop to preserve FIFO order"

let test_multiple_producers_and_consumers_preserve_every_value = fun _ctx ->
  let queue = Queue.create () in
  let producer_count = 4 in
  let consumer_count = 4 in
  let values_per_producer = 25 in
  let total = producer_count * values_per_producer in
  let parent = self () in
  let done_producers = Sync.Atomic.make 0 in
  for consumer = 0 to consumer_count - 1 do
    ignore
      (
        spawn
          (fun () ->
            let rec consume () =
              match Queue.pop queue with
              | Some value ->
                  send parent (Mpmc_value value);
                  consume ()
              | None ->
                  if Sync.Atomic.get done_producers = producer_count then (
                    send parent (Mpmc_consumer_done consumer);
                    Ok ()
                  ) else (
                    sleep (Time.Duration.from_millis 1);
                    consume ()
                  )
            in
            consume ())
      )
  done;
  for producer = 0 to producer_count - 1 do
    ignore
      (
        spawn
          (fun () ->
            for seq = 0 to values_per_producer - 1 do
              Queue.push queue ~value:((producer * 1_000) + seq)
            done;
            let _ = Sync.Atomic.fetch_and_add done_producers 1 in
            Ok ())
      )
  done;
  let seen = HashSet.create () in
  let rec collect remaining_values remaining_consumers =
    if remaining_values = 0 && remaining_consumers = 0 then
      Ok ()
    else
      match
        await ~what:"MPMC queue activity"
          (
            function
            | Mpmc_value value -> `select (`Value value)
            | Mpmc_consumer_done idx -> `select (`Done idx)
            | _ -> `skip
          )
      with
      | Error _ as error -> error
      | Ok (`Value value) ->
          ignore (HashSet.insert seen ~value);
          collect (remaining_values - 1) remaining_consumers
      | Ok (`Done _) -> collect remaining_values (remaining_consumers - 1)
  in
  match collect total consumer_count with
  | Error _ as error -> error
  | Ok () ->
      if not (Int.equal (HashSet.length seen) total) then
        Error "expected Queue.MPMC to deliver each value exactly once"
      else if not (Queue.is_empty queue) then
        Error "expected Queue.MPMC to be empty after producers and consumers complete"
      else
        Ok ()

let name = "Queue.MPMC"

let tests = [
  Test.case "Queue.MPMC preserves FIFO order" test_fifo;
  Test.case ~size:Large
    "Queue.MPMC preserves every value across concurrent producers and consumers"
    test_multiple_producers_and_consumers_preserve_every_value;
]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
