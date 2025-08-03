open Miniriot

type Message.t += A of int

let () =
  let received = ref [] in
  
  let worker () =
    (* Receive only even numbers first *)
    for _ = 1 to 3 do
      let n = selective_receive (function
        | A n when n mod 2 = 0 -> `select n
        | _ -> `skip) in
      received := n :: !received
    done;
    
    (* Now receive odd numbers *)
    for _ = 1 to 3 do
      let n = selective_receive (function
        | A n when n mod 2 = 1 -> `select n
        | _ -> `skip) in
      received := n :: !received
    done;
    
    Process.Normal
  in
  
  let main () =
    let pid = spawn worker in
    
    (* Send mixed even/odd numbers *)
    List.iter (fun n -> send pid (A n)) [1; 2; 3; 4; 5; 6];
    
    for _ = 1 to 10 do yield () done;
    
    (* Should receive [2,4,6] then [1,3,5], so reversed: [5,3,1,6,4,2] *)
    let expected = [5; 3; 1; 6; 4; 2] in
    if !received = expected then
      Process.Normal
    else (
      Printf.printf "Expected: %s\n" (String.concat ", " (List.map string_of_int expected));
      Printf.printf "Got: %s\n" (String.concat ", " (List.map string_of_int !received));
      Process.Exception (Failure "Message order not preserved")
    )
  in
  
  let status = run ~main in
  Printf.printf "selective_receive_queue: %s\n" (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status