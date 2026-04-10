(* Pattern guards in match expressions. *)
type token =
  | Int of int
  | Word of string

let classify = function
  | Int n when n mod 2 = 0 -> "even-int"
  | Int _ -> "odd-int"
  | Word s when String.length s = 0 -> "empty-word"
  | Word s when Char.uppercase_ascii s.[0] = s.[0] -> "capitalized"
  | Word _ -> "word"

let () =
  let xs = [ Int 3; Int 4; Word ""; Word "Raml"; Word "compiler" ] in
  List.iter (fun x -> Printf.printf "%s " (classify x)) xs;
  print_newline ()
