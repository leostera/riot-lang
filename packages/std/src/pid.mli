(** # Pid - Process identifiers

    Process identifiers used throughout the actors runtime.

    ## Example

    ```ocaml
    let current = Process.self () in
    ignore current
    ```
*)

(** Re-export of the core process identifier API from [Actors.Pid]. *)
include module type of Actors.Pid
