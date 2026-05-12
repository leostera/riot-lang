(* Backtracking search. *)
let count_solutions n =
  let cols = Array.make n false in
  let diag1 = Array.make (2 * n) false in
  let diag2 = Array.make (2 * n) false in
  let rec place row =
    if row = n then 1
    else
      let total = ref 0 in
      for col = 0 to n - 1 do
        let d1 = row + col in
        let d2 = row - col + n in
        if not cols.(col) && not diag1.(d1) && not diag2.(d2) then begin
          cols.(col) <- true;
          diag1.(d1) <- true;
          diag2.(d2) <- true;
          total := !total + place (row + 1);
          cols.(col) <- false;
          diag1.(d1) <- false;
          diag2.(d2) <- false
        end
      done;
      !total
  in
  place 0

let () = Printf.printf "%d\n" (count_solutions 8)
