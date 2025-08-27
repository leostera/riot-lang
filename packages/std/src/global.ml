(** Panic exception and function *)

let panic msg =
  let exception Panic of string in
  raise (Panic msg)

(** Get the number of available CPU cores for parallelism *)
let available_parallelism () =
  match Sys.os_type with
  | "Unix" -> (
      try
        let ic =
          Unix.open_process_in
            "nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4"
        in
        let cores = input_line ic |> int_of_string in
        ignore (Unix.close_process_in ic);
        cores
      with _ -> 4)
  | _ -> 4

let cpu_count = available_parallelism
let os_type () = Sys.os_type
let time () = Unix.time ()
let gettimeofday () = Unix.gettimeofday ()
let time_ms () = int_of_float (Unix.gettimeofday () *. 1000.)
