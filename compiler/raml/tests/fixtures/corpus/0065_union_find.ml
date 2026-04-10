(* Imperative union-find. *)
type t = {
  parent : int array;
  rank : int array;
}

let create n =
  {
    parent = Array.init n Fun.id;
    rank = Array.make n 0;
  }

let rec find t x =
  if t.parent.(x) = x then x
  else begin
    t.parent.(x) <- find t t.parent.(x);
    t.parent.(x)
  end

let union t a b =
  let ra = find t a in
  let rb = find t b in
  if ra <> rb then
    if t.rank.(ra) < t.rank.(rb) then
      t.parent.(ra) <- rb
    else if t.rank.(ra) > t.rank.(rb) then
      t.parent.(rb) <- ra
    else begin
      t.parent.(rb) <- ra;
      t.rank.(ra) <- t.rank.(ra) + 1
    end

let () =
  let uf = create 6 in
  union uf 0 1;
  union uf 1 2;
  union uf 3 4;
  Array.iteri
    (fun i _ -> Printf.printf "%d->%d " i (find uf i))
    uf.parent;
  print_newline ()
