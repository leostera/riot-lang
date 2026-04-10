(* A tiny bytecode virtual machine. *)
type instr =
  | Const of int
  | Add
  | Mul
  | Dup
  | Swap
  | Halt

let run program =
  let rec exec pc stack =
    match program.(pc), stack with
    | Halt, x :: _ -> x
    | Const n, _ -> exec (pc + 1) (n :: stack)
    | Add, x :: y :: tl -> exec (pc + 1) ((y + x) :: tl)
    | Mul, x :: y :: tl -> exec (pc + 1) ((y * x) :: tl)
    | Dup, x :: tl -> exec (pc + 1) (x :: x :: tl)
    | Swap, x :: y :: tl -> exec (pc + 1) (y :: x :: tl)
    | _ -> failwith "bad bytecode program"
  in
  exec 0 []

let program =
  [|
    Const 6;
    Const 7;
    Mul;
    Dup;
    Const 2;
    Add;
    Halt;
  |]

let () = Printf.printf "%d\n" (run program)
