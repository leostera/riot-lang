(** Panic exception and function *)

let panic msg =
  let exception Panic of string in
  raise (Panic msg)

(** Get the number of available CPU cores for parallelism *)
let available_parallelism () =
  match Kernel.System.os_type with
  | "Unix" -> (
      try
        let ic =
          Kernel.Osprocess.open_process_in
            "nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4"
        in
        let cores = input_line ic |> int_of_string in
        ignore (Kernel.Osprocess.close_process_in ic);
        cores
      with _ -> 4)
  | _ -> 4

let cpu_count = available_parallelism
let os_type () = Kernel.System.os_type
let time () = Kernel.Time.time ()
let gettimeofday () = Kernel.Time.gettimeofday ()
let time_ms () = int_of_float (Kernel.Time.gettimeofday () *. 1000.)

(** Create a mutable cell *)
let cell x = Cell.create x
