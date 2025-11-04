open Std

let () =
  (* Test size detection *)
  match Tty.Terminal.size () with
  | Ok (width, height) ->
      Printf.printf "SUCCESS: Terminal is %d columns x %d rows\n%!" width height;
      Printf.printf "Expected: 95 columns x 49 rows\n%!";
      if width = 95 && height = 49 then
        Printf.printf "✓ Size detection is CORRECT!\n%!"
      else
        Printf.printf "✗ Size detection is WRONG\n%!"
  | Error (`System_error msg) ->
      Printf.printf "ERROR: %s\n%!" msg;
      Printf.printf "This means the ioctl call failed.\n%!"
