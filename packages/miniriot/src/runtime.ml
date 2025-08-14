(* Runtime module for Riot - provides reduction counting for compiler instrumentation *)

(* Thread-local storage for reduction count *)
let reduction_count = ref 100

let reset_reductions n = 
  reduction_count := n

let increment_reduction_count () =
  reduction_count := !reduction_count - 1;
  if !reduction_count <= 0 then begin
    reduction_count := 100;  (* Reset for next scheduling slice *)
    Effects.yield ()
  end