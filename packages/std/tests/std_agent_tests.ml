open Std

let rec wait_until ~attempts ~delay ~fn =
  if fn () then
    true
  else if attempts <= 0 then
    false
  else (
    sleep delay;
    wait_until ~attempts:(attempts - 1) ~delay ~fn
  )

let test_agent_start_and_get = fun _ctx ->
  let agent = Agent.start ~fn:(fun () -> 41) in
  let actual = Agent.get agent ~fn:(fun value -> value) in
  Agent.stop agent;
  if Int.equal actual 41 then
    Ok ()
  else
    Error "expected Agent.get to return initial state"

let test_agent_update_and_get = fun _ctx ->
  let agent = Agent.start ~fn:(fun () -> 0) in
  Agent.update agent ~fn:(fun value -> value + 1);
  let actual = Agent.get agent ~fn:(fun value -> value) in
  Agent.stop agent;
  if Int.equal actual 1 then
    Ok ()
  else
    Error "expected Agent.update to persist new state"

let test_agent_get_and_update = fun _ctx ->
  let agent = Agent.start ~fn:(fun () -> 5) in
  let reply = Agent.get_and_update agent ~fn:(fun value -> (value * 2, value + 3)) in
  let actual = Agent.get agent ~fn:(fun value -> value) in
  Agent.stop agent;
  if Int.equal reply 10 && Int.equal actual 8 then
    Ok ()
  else
    Error "expected get_and_update to reply and persist atomically"

let test_agent_cast_eventually_updates = fun _ctx ->
  let agent = Agent.start ~fn:(fun () -> 10) in
  Agent.cast agent ~fn:(fun value -> value + 5);
  let updated =
    wait_until
      ~attempts:50
      ~delay:(Time.Duration.from_millis 10)
      ~fn:(fun () -> Int.equal (Agent.get agent ~fn:(fun value -> value)) 15)
  in
  Agent.stop agent;
  if updated then
    Ok ()
  else
    Error "expected Agent.cast to update state eventually"

let test_agent_start_link_supports_operations = fun _ctx ->
  let agent = Agent.start_link ~fn:(fun () -> "hello") in
  Agent.update agent ~fn:(fun value -> value ^ " world");
  let actual = Agent.get agent ~fn:(fun value -> value) in
  Agent.stop agent;
  if String.equal actual "hello world" then
    Ok ()
  else
    Error "expected linked agent to support get/update"

let tests =
  Test.[
    case "Agent.start then Agent.get returns initial state" test_agent_start_and_get;
    case "Agent.update then Agent.get reflects update" test_agent_update_and_get;
    case "Agent.get_and_update replies and stores new state" test_agent_get_and_update;
    case "Agent.cast eventually updates state" test_agent_cast_eventually_updates;
    case "Agent.start_link supports normal operations" test_agent_start_link_supports_operations;
  ]

let main ~args = Test.Cli.main ~name:"agent" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
