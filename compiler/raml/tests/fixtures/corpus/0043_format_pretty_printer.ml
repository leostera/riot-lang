(* Pretty-printing with Format boxes. *)
open Format

let pp_pair ppf (a, b) =
  fprintf ppf "@[(%d,@ %d)@]" a b

let pp_list pp ppf xs =
  fprintf ppf "@[[";
  let rec loop = function
    | [] -> ()
    | [ x ] -> pp ppf x
    | x :: xs ->
        pp ppf x;
        fprintf ppf ";@ ";
        loop xs
  in
  loop xs;
  fprintf ppf "]@]"

let () =
  printf "@[<v>%a@]@."
    (pp_list pp_pair)
    [ (1, 2); (3, 4); (5, 6) ]
