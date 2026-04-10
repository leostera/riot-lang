(* Custom comparison function for sorting. *)
type item = {
  key : int;
  label : string;
}

let cmp a b =
  match compare a.key b.key with
  | 0 -> String.compare a.label b.label
  | n -> n

let () =
  let items =
    [|
      { key = 2; label = "b" };
      { key = 1; label = "x" };
      { key = 2; label = "a" };
      { key = 1; label = "y" };
    |]
  in
  Array.sort cmp items;
  Array.iter (fun x -> Printf.printf "%d:%s " x.key x.label) items;
  print_newline ()
