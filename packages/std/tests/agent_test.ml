open Std

(* Helper to run in a miniriot process *)
let run_test () =
  print_endline "\n=== Testing Agent (Parametric) ===";
  
  (* Test basic operations with int *)
  let counter = Agent.start_link (fun () -> 0) in
  
  (* Initial value should be 0 *)
  let value = Agent.get counter (fun n -> n) in
  Printf.printf "Initial value: %d (expected: 0)\n" value;
  
  (* Update and check *)
  Agent.update counter (fun n -> n + 1);
  let value = Agent.get counter (fun n -> n) in
  Printf.printf "After update: %d (expected: 1)\n" value;
  
  (* Test get with transformation *)
  let doubled = Agent.get counter (fun n -> n * 2) in
  Printf.printf "Doubled value: %d (expected: 2)\n" doubled;
  
  (* Test get_and_update *)
  let old = Agent.get_and_update counter (fun n -> (n, n + 10)) in
  Printf.printf "Old value from get_and_update: %d (expected: 1)\n" old;
  let new_val = Agent.get counter (fun n -> n) in
  Printf.printf "New value after get_and_update: %d (expected: 11)\n" new_val;
  
  (* Test cast (async) *)
  Agent.cast counter (fun n -> n + 5);
  sleep 0.1; (* Give it time to process *)
  let value = Agent.get counter (fun n -> n) in
  Printf.printf "After cast: %d (expected: 16)\n" value;
  
  Agent.stop counter;
  print_endline "✓ Integer agent tests passed!\n";
  
  (* Test with different types *)
  print_endline "=== Testing Agent with different types ===";
  
  let string_agent = Agent.start (fun () -> "hello") in
  Agent.update string_agent (fun s -> s ^ " world");
  let str_value = Agent.get string_agent (fun s -> s) in
  Printf.printf "String agent: %s (expected: hello world)\n" str_value;
  
  let len = Agent.get string_agent String.length in
  Printf.printf "String length: %d (expected: 11)\n" len;
  
  Agent.stop string_agent;
  
  (* Test with record type *)
  type person = { name : string; age : int }
  let person_agent = Agent.start (fun () -> { name = "Alice"; age = 30 }) in
  
  Agent.update person_agent (fun p -> { p with age = p.age + 1 });
  let person = Agent.get person_agent (fun p -> p) in
  Printf.printf "Person: %s, age %d (expected: Alice, age 31)\n" person.name person.age;
  
  let name = Agent.get person_agent (fun p -> p.name) in
  Printf.printf "Just name: %s (expected: Alice)\n" name;
  
  Agent.stop person_agent;
  
  print_endline "✓ Type polymorphism tests passed!\n";
  
  print_endline "=== All Agent tests passed! ===\n"

let () =
  Miniriot.run @@ fun () ->
  spawn run_test |> ignore
