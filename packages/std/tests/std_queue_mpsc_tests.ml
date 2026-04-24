open Std
module Queue = Collections.Queue.MPSC
module HashSet = Collections.HashSet

type Message.t +=
  | Producer_done of int

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 1) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let sort_ints = fun values -> List.sort values ~compare:Int.compare

let wait_for_producers = fun ~producer_count ->
  let rec loop remaining =
    if remaining = 0 then
      Ok ()
    else
      match
        await ~what:"producer completion"
          (
            function
            | Producer_done _ -> `select ()
            | _ -> `skip
          )
      with
      | Ok () -> loop (remaining - 1)
      | Error _ as error -> error
  in
  loop producer_count

let test_fifo = fun _ctx ->
  let queue = Queue.create () in
  Queue.push queue ~value:1;
  Queue.push queue ~value:2;
  Queue.push queue ~value:3;
  match Queue.pop queue, Queue.pop queue, Queue.pop queue, Queue.pop queue with
  | Some 1, Some 2, Some 3, None -> Ok ()
  | _ -> Error "expected Queue.MPSC.pop to preserve FIFO order"

let test_multiple_producers_preserve_every_value = fun _ctx ->
  let queue = Queue.create () in
  let producer_count = 4 in
  let values_per_producer = 25 in
  let total = producer_count * values_per_producer in
  let parent = self () in
  for producer = 0 to producer_count - 1 do
    ignore
      (
        spawn
          (fun () ->
            for seq = 0 to values_per_producer - 1 do
              Queue.push queue ~value:((producer * 1_000) + seq)
            done;
            send parent (Producer_done producer);
            Ok ())
      )
  done;
  match wait_for_producers ~producer_count with
  | Error _ as error -> error
  | Ok () ->
      let rec drain acc =
        match Queue.pop queue with
        | None -> List.reverse acc
        | Some value -> drain (value :: acc)
      in
      let drained = drain [] in
      let seen = HashSet.from_list drained in
      if not (Int.equal (List.length drained) total) then
        Error "expected Queue.MPSC to keep every produced value"
      else if not (Int.equal (HashSet.length seen) total) then
        Error "expected Queue.MPSC not to duplicate values"
      else if not (Queue.is_empty queue) then
        Error "expected Queue.MPSC to be empty after draining"
      else
        let expected =
          let rec build producer seq acc =
            if producer = producer_count then
              acc
            else if seq = values_per_producer then
              build (producer + 1) 0 acc
            else
              build producer (seq + 1) (((producer * 1_000) + seq) :: acc)
          in
          build 0 0 []
        in
        if sort_ints drained = sort_ints expected then
          Ok ()
        else
          Error "expected Queue.MPSC to preserve the full produced set"

let name = "Queue.MPSC"

let tests = [
  Test.case "Queue.MPSC preserves FIFO order for one consumer" test_fifo;
  Test.case
    "Queue.MPSC preserves every value from concurrent producers"
    test_multiple_producers_preserve_every_value;
]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
