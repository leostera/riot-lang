open Std
open Collections

(* Internal: Sorted vector of unique elements *)
type 'a t = { elements : 'a Vector.t }

(* Construction *)

let empty () = { elements = Vector.create () }

let of_vec vec =
  (* Collect to list first *)
  let lst = ref [] in
  Vector.iter (fun x -> lst := x :: !lst) vec;
  let sorted_lst = List.sort compare !lst in
  (* Remove duplicates *)
  let deduped = Vector.create () in
  match sorted_lst with
  | [] -> { elements = deduped }
  | first :: rest ->
      let prev = cell first in
      Vector.push deduped first;
      List.iter
        (fun curr ->
          if compare curr (Sync.Cell.get prev) != 0 then (
            Vector.push deduped curr;
            Sync.Cell.set prev curr))
        rest;
      { elements = deduped }

let of_list lst =
  (* Sort and deduplicate list directly *)
  let sorted_lst = List.sort compare lst in
  let deduped = Vector.create () in
  match sorted_lst with
  | [] -> { elements = deduped }
  | first :: rest ->
      let prev = cell first in
      Vector.push deduped first;
      List.iter
        (fun curr ->
          if compare curr (Sync.Cell.get prev) != 0 then (
            Vector.push deduped curr;
            Sync.Cell.set prev curr))
        rest;
      { elements = deduped }

let singleton x =
  let vec = Vector.create () in
  Vector.push vec x;
  { elements = vec }

(* Access *)

let to_list rel =
  let lst = ref [] in
  Vector.iter (fun x -> lst := x :: !lst) rel.elements;
  List.rev !lst

let to_vec rel = rel.elements

let length rel = Vector.len rel.elements

let is_empty rel = Vector.is_empty rel.elements

(* Set Operations *)

let merge rel1 rel2 =
  (* Sorted merge of two sorted vectors *)
  let v1 = rel1.elements in
  let v2 = rel2.elements in
  let len1 = Vector.len v1 in
  let len2 = Vector.len v2 in
  let result = Vector.create () in

  let i = cell 0 in
  let j = cell 0 in

  while Sync.Cell.get i < len1 && Sync.Cell.get j < len2 do
    let x = Vector.get v1 (Sync.Cell.get i) |> Option.unwrap in
    let y = Vector.get v2 (Sync.Cell.get j) |> Option.unwrap in
    match compare x y with
    | c when c < 0 ->
        Vector.push result x;
        Sync.Cell.update i (fun n -> n + 1)
    | c when c > 0 ->
        Vector.push result y;
        Sync.Cell.update j (fun n -> n + 1)
    | _ ->
        (* Equal - only add once *)
        Vector.push result x;
        Sync.Cell.update i (fun n -> n + 1);
        Sync.Cell.update j (fun n -> n + 1)
  done;

  (* Add remaining elements *)
  while Sync.Cell.get i < len1 do
    Vector.push result (Vector.get v1 (Sync.Cell.get i) |> Option.unwrap);
    Sync.Cell.update i (fun n -> n + 1)
  done;

  while Sync.Cell.get j < len2 do
    Vector.push result (Vector.get v2 (Sync.Cell.get j) |> Option.unwrap);
    Sync.Cell.update j (fun n -> n + 1)
  done;

  { elements = result }

let diff rel1 rel2 =
  (* Elements in rel1 but not in rel2 *)
  let v1 = rel1.elements in
  let v2 = rel2.elements in
  let len1 = Vector.len v1 in
  let len2 = Vector.len v2 in
  let result = Vector.create () in

  let i = cell 0 in
  let j = cell 0 in

  while Sync.Cell.get i < len1 do
    if Sync.Cell.get j >= len2 then (
      (* No more elements in v2, add rest of v1 *)
      Vector.push result (Vector.get v1 (Sync.Cell.get i) |> Option.unwrap);
      Sync.Cell.update i (fun n -> n + 1))
    else
      let x = Vector.get v1 (Sync.Cell.get i) |> Option.unwrap in
      let y = Vector.get v2 (Sync.Cell.get j) |> Option.unwrap in
      match compare x y with
      | c when c < 0 ->
          (* x not in v2 *)
          Vector.push result x;
          Sync.Cell.update i (fun n -> n + 1)
      | c when c > 0 ->
          (* Skip y *)
          Sync.Cell.update j (fun n -> n + 1)
      | _ ->
          (* x = y, skip both *)
          Sync.Cell.update i (fun n -> n + 1);
          Sync.Cell.update j (fun n -> n + 1)
  done;

  { elements = result }

let intersect rel1 rel2 =
  (* Elements in both relations *)
  let v1 = rel1.elements in
  let v2 = rel2.elements in
  let len1 = Vector.len v1 in
  let len2 = Vector.len v2 in
  let result = Vector.create () in

  let i = cell 0 in
  let j = cell 0 in

  while Sync.Cell.get i < len1 && Sync.Cell.get j < len2 do
    let x = Vector.get v1 (Sync.Cell.get i) |> Option.unwrap in
    let y = Vector.get v2 (Sync.Cell.get j) |> Option.unwrap in
    match compare x y with
    | c when c < 0 -> Sync.Cell.update i (fun n -> n + 1)
    | c when c > 0 -> Sync.Cell.update j (fun n -> n + 1)
    | _ ->
        (* Found in both *)
        Vector.push result x;
        Sync.Cell.update i (fun n -> n + 1);
        Sync.Cell.update j (fun n -> n + 1)
  done;

  { elements = result }

(* Iteration *)

let iter f rel = Vector.iter f rel.elements

let fold f acc rel =
  let result = cell acc in
  Vector.iter (fun x -> Sync.Cell.set result (f (Sync.Cell.get result) x)) rel.elements;
  Sync.Cell.get result

let map f rel =
  let result = Vector.create () in
  Vector.iter (fun x -> Vector.push result (f x)) rel.elements;
  of_vec result

let filter f rel =
  let result = Vector.create () in
  Vector.iter (fun x -> if f x then Vector.push result x) rel.elements;
  { elements = result }

(* Search *)

let contains rel x =
  (* Binary search in sorted vector *)
  let vec = rel.elements in
  let len = Vector.len vec in
  let rec search low high =
    if low > high then false
    else
      let mid = (low + high) / 2 in
      let mid_val = Vector.get vec mid |> Option.unwrap in
      match compare x mid_val with
      | 0 -> true
      | c when c < 0 -> search low (mid - 1)
      | _ -> search (mid + 1) high
  in
  if len = 0 then false else search 0 (len - 1)

let find f rel =
  let vec = rel.elements in
  let len = Vector.len vec in
  let result = cell None in
  let i = cell 0 in
  while Sync.Cell.get i < len && Option.is_none (Sync.Cell.get result) do
    let x = Vector.get vec (Sync.Cell.get i) |> Option.unwrap in
    if f x then Sync.Cell.set result (Some x);
    Sync.Cell.update i (fun n -> n + 1)
  done;
  Sync.Cell.get result
