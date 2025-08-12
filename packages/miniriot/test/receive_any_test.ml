open Miniriot

type Message.t += A of int | B of string | C of float

let () =
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
  Printf.printf "receive_any: %s\n" (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status
