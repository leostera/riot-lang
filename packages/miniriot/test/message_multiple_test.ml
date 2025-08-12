open Miniriot

type Message.t += Count of int

let () =
  let messages = ref [] in

  let worker () =
    for _ = 1 to 3 do
      match receive () with Count n -> messages := n :: !messages | _ -> ()
    done;
    Process.Normal
  in

  let main () =
    let pid = spawn worker in
    send pid (Count 1);
    send pid (Count 2);
    send pid (Count 3);

    yield ();
    yield ();

    (* Let worker process *)
    let expected = [ 3; 2; 1 ] in
    (* Reverse order due to list cons *)
    if !messages = expected then Process.Normal
    else
      Process.Exception
        (Failure
           (Printf.sprintf "Expected %s, got %s"
              (String.concat ";" (List.map string_of_int expected))
              (String.concat ";" (List.map string_of_int !messages))))
  in

  let status = run ~main in
  Printf.printf "message_multiple: %s\n"
    (if status = 0 then "✓ PASS" else "✗ FAIL");
  Stdlib.exit status
