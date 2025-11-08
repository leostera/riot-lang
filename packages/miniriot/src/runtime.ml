(* Runtime module for Riot - provides reduction counting for compiler instrumentation *)

open Kernel
open Kernel.Sync
open Kernel.Sync.Cell

(* Thread-local storage for reduction count *)
let reduction_count = Cell.create 100
let reset_reductions n = reduction_count := n

let increment_reduction_count () =
  reduction_count := !reduction_count - 1;
  if !reduction_count <= 0 then (
    reduction_count := 100;
    (* Reset for next scheduling slice *)
    Effects.yield ())
