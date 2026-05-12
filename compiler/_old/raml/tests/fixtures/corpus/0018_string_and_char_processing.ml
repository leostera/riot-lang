(* String mapping and character classification. *)
let is_vowel = function
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false

let count_vowels s =
  let n = ref 0 in
  String.iter
    (fun c ->
      let c = Char.lowercase_ascii c in
      if is_vowel c then incr n)
    s;
  !n

let uppercase_ascii s =
  String.map Char.uppercase_ascii s

let () =
  Printf.printf "%s %d\n"
    (uppercase_ascii "raml rocks")
    (count_vowels "Compiler")
