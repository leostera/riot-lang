open Kernel.Global

include Kernel.Float

let rec power_of_10 = fun n ->
  match n with
  | 0 -> 1.0
  | 1 -> 10.0
  | 2 -> 100.0
  | 3 -> 1000.0
  | 4 -> 10000.0
  | 5 -> 100000.0
  | 6 -> 1000000.0
  | _ -> 10.0 *. power_of_10 (n - 1)

let to_string = fun ?(precision = 6) f ->
  let multiplier = power_of_10 precision in
  let truncated = round (f *. multiplier) /. multiplier in
  string_of_float truncated
