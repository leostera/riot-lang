(* Test that trivia is preserved around operators *)

(* Arithmetic operators with trivia *)
let a = 1 (* left *) + (* op *) 2 (* right *)
let b = 3 (* left *) - (* minus *) 4
let c = 5 (* left *) * (* times *) 6
let d = 7 (* left *) / (* divide *) 8
let e = 9 (* left *) mod (* modulo *) 2

(* Comparison operators *)
let f = 1 (* left *) = (* eq *) 2
let g = 3 (* left *) <> (* neq *) 4
let h = 5 (* left *) < (* lt *) 6
let i = 7 (* left *) > (* gt *) 8
let j = 9 (* left *) <= (* lte *) 10
let k = 11 (* left *) >= (* gte *) 12

(* Logical operators *)
let l = true (* left *) && (* and *) false
let m = true (* left *) || (* or *) false
let n = not (* not *) true

(* List operators *)
let o = 1 (* head *) :: (* cons *) [2; 3]
let p = [1] (* left *) @ (* append *) [2]

(* Pipe operators *)
let q = 1 (* value *) |> (* pipe *) succ
let r = succ (* fn *) @@ (* apply *) 1

(* Assignment *)
let x = ref 0
let () = x (* ref *) := (* assign *) 1 (* value *)

(* Sequential *)
let s = print_endline "first" (* first *) ; (* seq *) print_endline "second"
