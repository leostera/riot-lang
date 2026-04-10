(* Cleanup with Fun.protect. *)
let capture () =
  let buf = Buffer.create 16 in
  (try
     Fun.protect
       ~finally:(fun () -> Buffer.add_string buf "|finally")
       (fun () ->
         Buffer.add_string buf "body";
         raise Exit)
   with
   | Exit -> Buffer.add_string buf "|caught");
  Buffer.contents buf

let () = print_endline (capture ())
