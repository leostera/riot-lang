open Miniriot

type Message.t += A of int | B of string | C of float

let test_selective_receive_skip () =
  let received = ref [] in

  let worker () =
    (* First, receive only B messages *)
    let msg1 = selective_receive (function B s -> `select s | _ -> `skip) in
    received := ("B", msg1) :: !received;

    (* Then receive only A messages *)
    let msg2 = selective_receive (function A n -> `select n | _ -> `skip) in
    received := ("A", string_of_int msg2) :: !received;

    (* Finally receive C *)
    let msg3 = selective_receive (function C f -> `select f | _ -> `skip) in
    received := ("C", string_of_float msg3) :: !received;

    Process.Normal
  in

  let main () =
    let pid = spawn worker in

    (* Send messages in different order *)
    send pid (A 42);
    send pid (B "hello");
    send pid (C 3.14);

    for _ = 1 to 5 do
      yield ()
    done;

    (* Worker should receive B first, then A, then C *)
    let expected = [ ("C", "3.14"); ("A", "42"); ("B", "hello") ] in
    if !received = expected then Process.Normal
    else (
      Printf.printf "Expected: %s\n"
        (String.concat ", "
           (List.map (fun (t, v) -> Printf.sprintf "(%s,%s)" t v) expected));
      Printf.printf "Got: %s\n"
        (String.concat ", "
           (List.map (fun (t, v) -> Printf.sprintf "(%s,%s)" t v) !received));
      Process.Exception (Failure "Selective receive order wrong"))
  in

  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_selective_receive_skip\n"

let test_selective_receive_queue_preservation () =
  let received = ref [] in

  let worker () =
    (* Receive only even numbers first *)
    for _ = 1 to 3 do
      let n =
        selective_receive (function
          | A n when n mod 2 = 0 -> `select n
          | _ -> `skip)
      in
      received := n :: !received
    done;

    (* Now receive odd numbers *)
    for _ = 1 to 3 do
      let n =
        selective_receive (function
          | A n when n mod 2 = 1 -> `select n
          | _ -> `skip)
      in
      received := n :: !received
    done;

    Process.Normal
  in

  let main () =
    let pid = spawn worker in

    (* Send mixed even/odd numbers *)
    List.iter (fun n -> send pid (A n)) [ 1; 2; 3; 4; 5; 6 ];

    for _ = 1 to 10 do
      yield ()
    done;

    (* Should receive [2,4,6] then [1,3,5], so reversed: [5,3,1,6,4,2] *)
    let expected = [ 5; 3; 1; 6; 4; 2 ] in
    if !received = expected then Process.Normal
    else (
      Printf.printf "Expected: %s\n"
        (String.concat ", " (List.map string_of_int expected));
      Printf.printf "Got: %s\n"
        (String.concat ", " (List.map string_of_int !received));
      Process.Exception (Failure "Message order not preserved"))
  in

  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_selective_receive_queue_preservation\n"

let test_receive_any_message () =
  let messages = ref [] in

  let worker () =
    for _ = 1 to 3 do
      let msg = receive () in
      match msg with
      | A n -> messages := Printf.sprintf "A(%d)" n :: !messages
      | B s -> messages := Printf.sprintf "B(%s)" s :: !messages
      | C f -> messages := Printf.sprintf "C(%g)" f :: !messages
      | _ -> ()
    done;
    Process.Normal
  in

  let main () =
    let pid = spawn worker in

    send pid (A 1);
    send pid (B "two");
    send pid (C 3.0);

    for _ = 1 to 5 do
      yield ()
    done;

    (* Should receive in order sent (reversed due to cons) *)
    let expected = [ "C(3)"; "B(two)"; "A(1)" ] in
    if !messages = expected then Process.Normal
    else Process.Exception (Failure "receive() didn't get all messages")
  in

  let status = run ~main in
  assert (status = 0);
  Printf.printf "✓ test_receive_any_message\n"

let () =
  Printf.printf "=== Selective Receive Tests ===\n";
  test_selective_receive_skip ();
  test_selective_receive_queue_preservation ();
  test_receive_any_message ();
  Printf.printf "All selective receive tests passed!\n"
