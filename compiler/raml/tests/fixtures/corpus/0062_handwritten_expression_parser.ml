(* Hand-written recursive descent parser. *)
exception Parse_error of string

let parse s =
  let len = String.length s in
  let i = ref 0 in
  let peek () =
    if !i < len then Some s.[!i] else None
  in
  let rec skip () =
    while !i < len && s.[!i] = ' ' do
      incr i
    done
  and eat c =
    skip ();
    match peek () with
    | Some d when d = c -> incr i
    | _ -> raise (Parse_error ("expected " ^ String.make 1 c))
  and number acc =
    match peek () with
    | Some c when c >= '0' && c <= '9' ->
        incr i;
        number ((acc * 10) + Char.code c - Char.code '0')
    | _ -> acc
  and factor () =
    skip ();
    match peek () with
    | Some '(' ->
        incr i;
        let v = expr () in
        eat ')';
        v
    | Some c when c >= '0' && c <= '9' -> number 0
    | _ -> raise (Parse_error "bad factor")
  and term_tail acc =
    skip ();
    match peek () with
    | Some '*' ->
        incr i;
        let v = factor () in
        term_tail (acc * v)
    | _ -> acc
  and term () =
    factor () |> term_tail
  and expr_tail acc =
    skip ();
    match peek () with
    | Some '+' ->
        incr i;
        let v = term () in
        expr_tail (acc + v)
    | _ -> acc
  and expr () =
    term () |> expr_tail
  in
  let v = expr () in
  skip ();
  if !i <> len then raise (Parse_error "trailing junk");
  v

let () = Printf.printf "%d\n" (parse "2 + 3 * (4 + 5)")
