(* Variants with inline records. *)
type instruction =
  | Push of { value : int }
  | Add
  | Dup

let run program =
  let rec exec stack = function
    | [] -> stack
    | Push { value } :: xs -> exec (value :: stack) xs
    | Add :: xs ->
        begin match stack with
        | a :: b :: tl -> exec ((a + b) :: tl) xs
        | _ -> failwith "stack underflow"
        end
    | Dup :: xs ->
        begin match stack with
        | a :: tl -> exec (a :: a :: tl) xs
        | _ -> failwith "stack underflow"
        end
  in
  exec [] program

let () =
  match run [ Push { value = 20 }; Dup; Push { value = 2 }; Add ] with
  | x :: _ -> Printf.printf "%d\n" x
  | [] -> print_endline "empty"
