(* Test trailing semicolons in list, array, and record literals *)

(* === LISTS === *)

(* Single element with trailing semicolon *)

let single_list = [ 1; ]

(* Multiple elements with trailing semicolon *)

let multiple_list = [ 1; 2; 3; ]

(* Empty list - no trailing semicolon possible *)

let empty_list = []

(* Nested lists with trailing semicolons *)

let nested_list = [ [ 1; 2; ]; [ 3; 4; ]; ]

(* List with complex expressions and trailing semicolon *)

let complex_list = [ 1 + 2; 3 * 4; 5 - 6; ]

(* List in function argument *)

let _ =
  List.map (fun x -> x + 1) [ 1; 2; 3; ]

(* Pattern matching with list containing trailing semicolon *)

let test_list x =
  match x with
  | [ a;  ] -> a
  | [a;b;] -> a + b
  | _ -> 0

(* === ARRAYS === *)

(* Single element with trailing semicolon *)

let single_array = [|1;|]

(* Multiple elements with trailing semicolon *)

let multiple_array = [|1; 2; 3;|]

(* Empty array - no trailing semicolon possible *)

let empty_array = [||]

(* Nested arrays with trailing semicolons *)

let nested_array = [|[|1; 2;|]; [|3; 4;|];|]

(* === SEQUENCES IN DIFFERENT CONTEXTS === *)

(* Sequence in list with trailing semicolon - each list element is a parenthesized sequence *)

let seq_in_list = [ (
    print_endline "a";
    1
  ); (
    print_endline "b";
    2
  ); ]

(* Sequence in array with trailing semicolon *)

let seq_in_array = [|(
    print_endline "a";
    1
  ); (
    print_endline "b";
    2
  );|]

(* List with let..in expressions *)

let let_in_list = [ (
    let x = 1 in
    x + 1
  ); (
    let y = 2 in
    y + 2
  ); ]
