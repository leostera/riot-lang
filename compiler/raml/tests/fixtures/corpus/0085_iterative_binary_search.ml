(* Imperative binary search over sorted arrays. *)
let binary_search a target =
  let lo = ref 0 in
  let hi = ref (Array.length a - 1) in
  let answer = ref None in
  while !lo <= !hi do
    let mid = !lo + ((!hi - !lo) / 2) in
    let v = a.(mid) in
    if v = target then begin
      answer := Some mid;
      lo := !hi + 1
    end else if v < target then
      lo := mid + 1
    else
      hi := mid - 1
  done;
  !answer

let () =
  let a = [| 1; 4; 7; 9; 12; 20 |] in
  let show = function None -> "none" | Some i -> string_of_int i in
  Printf.printf "%s %s\n"
    (show (binary_search a 9))
    (show (binary_search a 8))
