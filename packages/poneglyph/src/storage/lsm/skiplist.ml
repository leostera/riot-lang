(** SkipList - Probabilistic balanced search structure *)

open Std
open Std.Collections
open Std.IO

(** Maximum number of levels in the skip list *)
let max_level = 16

(** Probability for level promotion (p = 1/4 is optimal) *)
let p = 0.25

(** Node in the skip list *)
type node = {
  key : bytes;
  mutable value : bytes;  (* Mutable for updates *)
  forward : node option Array.t;
}

(** SkipList structure *)
type t = {
  header : node;
  mutable level : int;
  mutable size : int;
  mutable size_bytes : int;
}

(** Create a new empty skip list *)
let create () =
  let max_key = Bytes.make 41 '\xFF' in
  let header = {
    key = max_key;
    value = Bytes.empty;
    forward = Array.make max_level None;
  } in
  {
    header;
    level = 0;
    size = 0;
    size_bytes = 0;
  }

(** Generate random level for new node *)
let random_level () =
  let rec gen_level lvl =
    if lvl >= max_level - 1 then lvl
    else if Random.float 1.0 < p then gen_level (lvl + 1)
    else lvl
  in
  gen_level 0

(** Find the position to insert a key *)
let find_update_path t key =
  let update = Array.make max_level None in
  let current = ref (Some t.header) in
  
  for i = t.level downto 0 do
    let rec advance () =
      match !current with
      | None -> ()
      | Some node -> (
          match Array.get node.forward i with
          | None -> ()
          | Some next ->
              if Bytes.compare next.key key < 0 then (
                current := Some next;
                advance ()
              )
        )
    in
    advance ();
    Array.set update i !current;
  done;
  
  update

(** Insert a key-value pair *)
let insert t ~key ~value =
  if Bytes.length key != 41 then
    Error "Key must be exactly 41 bytes"
  else (
    let update = find_update_path t key in
    
    (* Check if key already exists *)
    let existing = match Array.get update 0 with
      | None -> None
      | Some node -> (
          match Array.get node.forward 0 with
          | None -> None
          | Some next ->
              if Bytes.compare next.key key = 0 then Some next
              else None
        )
    in
    
    match existing with
    | Some node ->
        (* Update existing node *)
        let old_size = 41 + Bytes.length node.value in
        let new_size = 41 + Bytes.length value in
        node.value <- value;
        t.size_bytes <- t.size_bytes - old_size + new_size;
        Ok false
    
    | None ->
        (* Insert new node *)
        let new_level = random_level () in
        
        if new_level > t.level then (
          for i = t.level + 1 to new_level do
            Array.set update i (Some t.header);
          done;
          t.level <- new_level;
        );
        
        let new_node = {
          key;
          value;
          forward = Array.make max_level None;
        } in
        
        for i = 0 to new_level do
          match Array.get update i with
          | None -> ()
          | Some pred ->
              Array.set new_node.forward i (Array.get pred.forward i);
              Array.set pred.forward i (Some new_node);
        done;
        
        t.size <- t.size + 1;
        let entry_size = 41 + Bytes.length value in
        t.size_bytes <- t.size_bytes + entry_size;
        
        Ok true
  )

(** Lookup a value by key *)
let find t ~key =
  if Bytes.length key != 41 then None
  else (
    let current = ref (Some t.header) in
    let key_hex = Data.Base16.encode_bytes key in
    
    for i = t.level downto 0 do
      let rec advance () =
        match !current with
        | None -> ()
        | Some node -> (
            match Array.get node.forward i with
            | None -> ()
            | Some next ->
                let cmp = Bytes.compare next.key key in
                if cmp < 0 then (
                  current := Some next;
                  advance ()
                ) else if cmp = 0 then
                  current := Some next
          )
      in
      advance ();
    done;
    
    match !current with
    | None -> None
    | Some node ->
        if Bytes.compare node.key key = 0 then Some node.value
        else None
  )

(** Get current size in bytes *)
let size_bytes t = t.size_bytes

(** Get number of entries *)
let count t = t.size

(** Iterate over all entries in sorted order *)
let iter t ~f =
  let rec visit node =
    match node with
    | None -> ()
    | Some n ->
        (* Skip only the header sentinel (all 0xFF bytes), not legitimate keys starting with 0xFF *)
        let is_header = Bytes.length n.key = 41 && Bytes.compare n.key t.header.key = 0 in
        
        if not is_header then
          f ~key:n.key ~value:n.value;
        (* Always continue to next node *)
        visit (Array.get n.forward 0)
  in
  visit (Array.get t.header.forward 0)

(** Fold over all entries in sorted order *)
let fold t ~init ~f =
  let acc = ref init in
  iter t ~f:(fun ~key ~value ->
    acc := f ~acc:!acc ~key ~value
  );
  !acc

(** Clear all entries *)
let clear t =
  for i = 0 to max_level - 1 do
    Array.set t.header.forward i None;
  done;
  t.level <- 0;
  t.size <- 0;
  t.size_bytes <- 0
