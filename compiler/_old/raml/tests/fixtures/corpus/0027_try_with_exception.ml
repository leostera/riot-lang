(* Custom exceptions and recovery. *)
exception Divide_by_zero_string

let safe_div_string a b =
  try
    if b = 0 then raise Divide_by_zero_string;
    string_of_int (a / b)
  with
  | Divide_by_zero_string -> "inf"

let () =
  Printf.printf "%s %s\n"
    (safe_div_string 12 3)
    (safe_div_string 12 0)
