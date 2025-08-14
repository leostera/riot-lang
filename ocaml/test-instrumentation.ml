(* Test script to verify Riot compiler instrumentation *)

(* Mock the Riot.Runtime module for testing *)
module Riot = struct
  module Runtime = struct
    let counter = ref 0
    
    let increment_reduction_count () =
      incr counter;
      if !counter mod 100 = 0 then
        Printf.printf "✓ Reduction count: %d\n%!" !counter
  end
end

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let test_function_application () =
  Printf.printf "\n=== Testing function applications ===\n";
  let result = factorial 10 in
  Printf.printf "factorial 10 = %d\n" result;
  Printf.printf "Reductions after factorial: %d\n" !(Riot.Runtime.counter)

let test_loops () =
  Printf.printf "\n=== Testing loops ===\n";
  let before = !(Riot.Runtime.counter) in
  
  (* Test while loop *)
  let i = ref 0 in
  while !i < 10 do
    incr i
  done;
  
  let after_while = !(Riot.Runtime.counter) in
  Printf.printf "While loop (10 iterations) added %d reductions\n" 
    (after_while - before);
  
  (* Test for loop *)
  for j = 1 to 10 do
    ignore (j + 1)
  done;
  
  let after_for = !(Riot.Runtime.counter) in
  Printf.printf "For loop (10 iterations) added %d reductions\n" 
    (after_for - after_while)

let test_operators () =
  Printf.printf "\n=== Testing operators ===\n";
  let before = !(Riot.Runtime.counter) in
  
  let _ = 1 + 2 in
  let _ = 3 * 4 in
  let _ = 10 / 2 in
  let _ = [1; 2; 3] in
  let _ = not true in
  let _ = !( ref 42) in
  
  let after = !(Riot.Runtime.counter) in
  Printf.printf "Operators added %d reductions\n" (after - before)

let () =
  Printf.printf "🧪 Testing Riot Compiler Instrumentation\n";
  Printf.printf "========================================\n";
  
  test_function_application ();
  test_loops ();
  test_operators ();
  
  Printf.printf "\n========================================\n";
  Printf.printf "Total reductions: %d\n" !(Riot.Runtime.counter);
  Printf.printf "✅ If you see reduction counts above, instrumentation is working!\n"