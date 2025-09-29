type 'a t = ('a, unit) Hashtbl.t

let create () = Hashtbl.create 16
let with_capacity capacity = Hashtbl.create capacity

let of_list elements =
  let set = Hashtbl.create (List.length elements) in
  List.iter (fun elem -> Hashtbl.replace set elem ()) elements;
  set

let insert set value =
  let was_present = Hashtbl.mem set value in
  Hashtbl.replace set value ();
  not was_present

let remove set value =
  let was_present = Hashtbl.mem set value in
  Hashtbl.remove set value;
  was_present

let contains set value = Hashtbl.mem set value
let len set = Hashtbl.length set
let is_empty set = Hashtbl.length set = 0
let clear set = Hashtbl.clear set
let iter f set = Hashtbl.iter (fun elem _ -> f elem) set
let fold f set acc = Hashtbl.fold (fun elem _ acc -> f elem acc) set acc
let to_list set = Hashtbl.fold (fun elem _ acc -> elem :: acc) set []

let union set1 set2 =
  let result = Hashtbl.copy set1 in
  Hashtbl.iter (fun elem _ -> Hashtbl.replace result elem ()) set2;
  result

let intersection set1 set2 =
  let result = Hashtbl.create 16 in
  Hashtbl.iter
    (fun elem _ -> if Hashtbl.mem set2 elem then Hashtbl.replace result elem ())
    set1;
  result

let difference set1 set2 =
  let result = Hashtbl.create 16 in
  Hashtbl.iter
    (fun elem _ ->
      if not (Hashtbl.mem set2 elem) then Hashtbl.replace result elem ())
    set1;
  result

let symmetric_difference set1 set2 =
  let result = Hashtbl.create 16 in
  Hashtbl.iter
    (fun elem _ ->
      if not (Hashtbl.mem set2 elem) then Hashtbl.replace result elem ())
    set1;
  Hashtbl.iter
    (fun elem _ ->
      if not (Hashtbl.mem set1 elem) then Hashtbl.replace result elem ())
    set2;
  result

let is_subset set1 set2 =
  Hashtbl.fold (fun elem _ acc -> acc && Hashtbl.mem set2 elem) set1 true

let is_superset set1 set2 = is_subset set2 set1

let is_disjoint set1 set2 =
  Hashtbl.fold (fun elem _ acc -> acc && not (Hashtbl.mem set2 elem)) set1 true
