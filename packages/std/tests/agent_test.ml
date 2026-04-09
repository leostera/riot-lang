open Std

type person = {
  name: string;
  age: int;
}

(* Helper to run in a actors process *)

let run_test = fun () ->
  println "\n=== Testing Agent (Parametric) ===";
  (* Test basic operations with int *)
  let counter =
    Agent.start_link (fun () -> 0)
  in
  (* Initial value should be 0 *)
  let value =
    Agent.get counter (fun n -> n)
  in
  println ("Initial value: " ^ string_of_int value ^ " (expected: 0)");
  (* Update and check *)
  Agent.update counter (fun n -> n + 1);
  let value =
    Agent.get counter (fun n -> n)
  in
  println ("After update: " ^ string_of_int value ^ " (expected: 1)");
  (* Test get with transformation *)
  let doubled =
    Agent.get counter (fun n -> n * 2)
  in
  println ("Doubled value: " ^ string_of_int doubled ^ " (expected: 2)");
  (* Test get_and_update *)
  let old =
    Agent.get_and_update counter (fun n -> (n, n + 10))
  in
  println ("Old value from get_and_update: " ^ string_of_int old ^ " (expected: 1)");
  let new_val =
    Agent.get counter (fun n -> n)
  in
  println ("New value after get_and_update: " ^ string_of_int new_val ^ " (expected: 11)");
  (* Test cast (async) *)
  Agent.cast counter (fun n -> n + 5);
  sleep 0.1;
  (* Give it time to process *)
  let value =
    Agent.get counter (fun n -> n)
  in
  println ("After cast: " ^ string_of_int value ^ " (expected: 16)");
  Agent.stop counter;
  println "✓ Integer agent tests passed!\n";
  (* Test with different types *)
  println "=== Testing Agent with different types ===";
  let string_agent =
    Agent.start (fun () -> "hello")
  in
  Agent.update string_agent (fun s -> s ^ " world");
  let str_value =
    Agent.get string_agent (fun s -> s)
  in
  println ("String agent: " ^ str_value ^ " (expected: hello world)");
  let len = Agent.get string_agent String.length in
  println ("String length: " ^ string_of_int len ^ " (expected: 11)");
  Agent.stop string_agent;
  (* Test with record type *)
  let person_agent =
    Agent.start (fun () -> { name = "Alice"; age = 30 })
  in
  Agent.update person_agent (fun p -> { p with age = p.age + 1 });
  let person =
    Agent.get person_agent (fun p -> p)
  in
  println
    ("Person: " ^ person.name ^ ", age " ^ string_of_int person.age ^ " (expected: Alice, age 31)");
  let name =
    Agent.get person_agent (fun p -> p.name)
  in
  println ("Just name: " ^ name ^ " (expected: Alice)");
  Agent.stop person_agent;
  println "✓ Type polymorphism tests passed!\n";
  println "=== All Agent tests passed! ===\n"

let () = Runtime.run @@ fun () -> spawn run_test |> ignore
