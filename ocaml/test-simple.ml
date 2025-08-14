(* Simple test to see instrumentation in parse tree *)

let add x y = x + y

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let () =
  let result = add 1 2 in
  let fact = factorial 5 in
  Printf.printf "add 1 2 = %d\n" result;
  Printf.printf "factorial 5 = %d\n" fact;
  
  for i = 1 to 3 do
    Printf.printf "Loop %d\n" i
  done;
  
  let j = ref 0 in
  while !j < 3 do
    Printf.printf "While %d\n" !j;
    incr j
  done