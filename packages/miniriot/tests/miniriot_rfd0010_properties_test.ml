open Miniriot
open Propane

let failwith msg = Kernel.panic msg

let concat parts = Kernel.String.concat "" parts
let int_to_string n = Kernel.Int.to_string n
let int_equal a b = Kernel.Int.equal a b
let int_max a b = Kernel.Int.max a b
let int_abs n = Kernel.Int.abs n

type Message.t += Probe_uid

let assert_ok name result =
  match result with
  | Property.Success -> ()
  | Property.Failure { counter_example; shrink_steps } ->
      failwith
        (concat
           [
             name;
             " failed\nCounter-example: ";
             counter_example;
             "\nShrink steps: ";
             int_to_string shrink_steps;
           ])
  | Property.Error { exception_; backtrace } ->
      let msg =
        match exception_ with
        | exn -> concat [ Printexc.to_string exn; "\n"; backtrace ]
      in
      failwith (concat [ name; " raised exception:\n"; msg ])
  | Property.Assumption_violated ->
      failwith (concat [ name; " had no applicable test cases" ])

let message_uids_increase : Property.test_property =
  Property.for_all
    (Arbitrary.int)
    (fun raw_n ->
      let n = ((int_abs raw_n) mod 64) + 1 in
      let ids =
        List.init n (fun _ -> Message.envelope Probe_uid |> fun e -> e.uid)
      in
      let rec strictly_increasing = function
        | a :: b :: rest -> a < b && strictly_increasing (b :: rest)
        | _ -> true
      in
      strictly_increasing ids)

let scheduler_count_clamped : Property.test_property =
  Property.for_all Arbitrary.int (fun n ->
      let cfg = Config.make ~scheduler_count:n () in
      int_equal cfg.scheduler_count (int_max 1 n))

let () =
  assert_ok "message UID monotonicity" (Property.check message_uids_increase);
  assert_ok "scheduler_count clamping" (Property.check scheduler_count_clamped)
