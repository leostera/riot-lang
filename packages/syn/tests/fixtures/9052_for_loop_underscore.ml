(* For loops with underscore as loop variable *)

let () =
  for _ = 1 to 10 do
    print_endline "hello"
  done

(* Downto variant *)

let () =
  for _ = 10 downto 1 do
    print_endline "bye"
  done
