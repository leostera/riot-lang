open Miniriot

type Message.t += Hello of string

let () =
  let received = ref None in

  let worker () =
    match receive () with
    | Hello msg ->
        received := Some msg;
        Process.Normal
    | _ -> Process.Exception (Failure "Unexpected message")
  in

  let main () =
    let pid = spawn worker in
    send pid (Hello "world");
    yield ();
    (* Let worker start *)
    yield ();
    (* Let worker receive *)
    yield ();

    (* Let worker finish *)
    match !received with
    | Some "world" -> Process.Normal
    | _ -> Process.Exception (Failure "Message not received correctly")
  in

  let status = run ~main in
  Printf.printf "Single message test: %s (status=%d)\n"
    (if status = 0 then "✓ PASS" else "✗ FAIL")
    status
