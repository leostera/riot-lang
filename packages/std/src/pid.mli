(**
   Process identifiers.

   Process identifiers used throughout the actors runtime.

   ## Example

   ```ocaml
   let current = Process.self () in
   ignore current
   ```
*)

(** Re-export of the core process identifier API from [Runtime.Pid]. *)
include module type of Runtime.Pid
