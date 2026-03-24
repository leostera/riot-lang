(* TODO(@leostera): we need to add more examples here for:
   - [ ] functions over records
   - [ ] inline pattern matching on `fun .. -> ..`
   - [ ] functions with large numbers of branches
   - [ ] fun's with large numbers of params (from 3 to 30)
   - [ ] functions that return functions
   - [ ] `let foo .. = ..` syntax is also missing here
*)

let fun_nested =
  fun x -> fun y -> x + y

let fun_body_match =
  fun value ->
    match value with
    | Some x -> x
    | None -> 0

let match_simple =
  match value with
  | 0 -> "zero"
  | n -> Int.to_string n

let function_one_case =
  function
  | [] -> 0

let function_two_cases =
  function
  | [] -> 0
  | x :: _ -> x

let function_tuple =
  function
  | x, y -> x + y

let function_list =
  function
  | [] -> 0
  | [x] -> x
  | x :: xs -> x + List.length xs

let function_constructor =
  function
  | Some x -> x
  | None -> 0

let function_or_pattern =
  function
  | `A | `B -> 1
  | `C -> 2

let function_when_guard =
  function
  | x when x > 0 -> x
  | _ -> 0

let function_nested_pattern =
  function
  | Some (x, Some y) -> x + y
  | _ -> 0

let function_application =
  List.map
    (function
      | Some x -> x
      | None -> 0)
    values
