open Miniriot

type Message.t += A of int | B of string | C of float

let () =
  let received = ref [] in
  
  let worker () =
    (* First, receive only B messages *)
    let msg1 = selective_receive (function
      | B s -> `select s
      | _ -> `skip) in
    received := ("B", msg1) :: !received;
    
    (* Then receive only A messages *)
    let msg2 = selective_receive (function
      | A n -> `select n
      | _ -> `skip) in
    received := ("A", string_of_int msg2) :: !received;
    
    (* Finally receive C *)
    let msg3 = selective_receive (function
      | C f -> `select f
      | _ -> `skip) in
    received := ("C", string_of_float msg3) :: !received;
    
    Process.Normal
  in
  
  let main () =
    let pid = spawn worker in
    
    (* Send messages in different order *)
    send pid (A 42);
    send pid (B "hello");
    send pid (C 3.14);
    
    for _ = 1 to 5 do yield () done;
    
    (* Worker should receive B first, then A, then C *)
    let expected = [("C", "3.14"); ("A", "42"); ("B", "hello")] in
    if !received = expected then
      Process.Normal
    else (
      Printf.printf "Expected: %s\n" 
        (String.concat ", " (List.map (fun (t,v) -> Printf.sprintf "(%s,%s)" t v) expected));
      Printf.printf "Got: %s\n"
        (String.concat ", " (List.map (fun (t,v) -> Printf.sprintf "(%s,%s)" t v) !received));
      Process.Exception (Failure "Selective receive order wrong")
    )
  in
  
  let status = run ~main in
  Printf.printf "selective_receive_skip: %s\n" (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status