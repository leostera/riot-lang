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
    (* DEBUG: Log FE insertions *)
    let key_hex = Data.Base16.encode_bytes key in
    if String.starts_with ~prefix:"FE8D5D99" key_hex then begin
      Log.info ("[SKIPLIST-INSERT] Inserting FE8D5D99 key");
      Log.info ("[SKIPLIST-INSERT] Header key: " ^ Data.Base16.encode_bytes t.header.key);
      Log.info ("[SKIPLIST-INSERT] Current skiplist size: " ^ string_of_int t.size);
      let header_fwd0_before = Array.get t.header.forward 0 in
      match header_fwd0_before with
      | None -> Log.info ("[SKIPLIST-INSERT] BEFORE: Header.forward[0] = None")
      | Some n ->
          let fwd_key_hex = Data.Base16.encode_bytes n.key in
          Log.info ("[SKIPLIST-INSERT] BEFORE: Header.forward[0] = " ^ String.sub fwd_key_hex 0 16 ^ "...")
    end;
    
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
        if String.starts_with ~prefix:"FE8D5D99" key_hex then
          Log.info ("[SKIPLIST-INSERT] FE8D5D99 is NEW (not updating existing)");
        
        let new_level = random_level () in
        
        if String.starts_with ~prefix:"FE8D5D99" key_hex then
          Log.info ("[SKIPLIST-INSERT] FE8D5D99 will be inserted at level " ^ string_of_int new_level);
        
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
        
        (* DEBUG: Verify linking for FE key *)
        if String.starts_with ~prefix:"FE8D5D99" key_hex then begin
          Log.info ("[SKIPLIST-INSERT] FE8D5D99 linking complete");
          
          (* Check new node's forward pointer *)
          let new_node_fwd0 = Array.get new_node.forward 0 in
          match new_node_fwd0 with
          | None -> Log.info ("[SKIPLIST-INSERT] FE8D5D99.forward[0] = None")
          | Some n ->
              let fwd_key_hex = Data.Base16.encode_bytes n.key in
              Log.info ("[SKIPLIST-INSERT] FE8D5D99.forward[0] points to: " ^ String.sub fwd_key_hex 0 16 ^ "...");
          
          (* Check header *)
          let header_fwd0 = Array.get t.header.forward 0 in
          match header_fwd0 with
          | None -> Log.info ("[SKIPLIST-INSERT] Header.forward[0] = None")
          | Some n -> 
              let fwd_key_hex = Data.Base16.encode_bytes n.key in
              Log.info ("[SKIPLIST-INSERT] Header.forward[0] points to: " ^ fwd_key_hex)
        end;
        
        t.size <- t.size + 1;
        let entry_size = 41 + Bytes.length value in
        t.size_bytes <- t.size_bytes + entry_size;
        
        Ok true
  )

(** Lookup a value by key *)
let find t ~key =
  if Bytes.length key != 41 then None
  else (
    (* DEBUG: Log FE lookups *)
    let key_hex = Data.Base16.encode_bytes key in
    if String.starts_with ~prefix:"FE8D5D99" key_hex then begin
      Log.info ("[SKIPLIST-FIND] Looking for FE8D5D99 key");
      let header_fwd0 = Array.get t.header.forward 0 in
      match header_fwd0 with
      | None -> Log.info ("[SKIPLIST-FIND] Header.forward[0] = None")
      | Some n ->
          let fwd_key_hex = Data.Base16.encode_bytes n.key in
          Log.info ("[SKIPLIST-FIND] Header.forward[0] points to: " ^ fwd_key_hex)
    end;
    
    let current = ref (Some t.header) in
    
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
    | None ->
        if String.starts_with ~prefix:"FE8D5D99" key_hex then
          Log.info ("[SKIPLIST-FIND] FE8D5D99 result: None (current is None)");
        None
    | Some node ->
        if Bytes.compare node.key key = 0 then begin
          if String.starts_with ~prefix:"FE8D5D99" key_hex then
            Log.info ("[SKIPLIST-FIND] FE8D5D99 result: FOUND");
          Some node.value
        end else begin
          if String.starts_with ~prefix:"FE8D5D99" key_hex then begin
            let node_key_hex = Data.Base16.encode_bytes node.key in
            Log.info ("[SKIPLIST-FIND] FE8D5D99 result: NOT FOUND (landed on key: " ^ node_key_hex ^ ")")
          end;
          None
        end
  )

(** Get current size in bytes *)
let size_bytes t = t.size_bytes

(** Get number of entries *)
let count t = t.size

(** Iterate over all entries in sorted order *)
let iter t ~f =
  (* DEBUG: Track iteration *)
  let count = ref 0 in
  let found_fe = ref false in
  
  let last_key = ref None in
  
  let rec visit node =
    match node with
    | None ->
        (* DEBUG: Log where iteration stopped *)
        (match !last_key with
        | None -> ()
        | Some k ->
            let key_hex = Data.Base16.encode_bytes k in
            if String.starts_with ~prefix:"FD" key_hex || String.starts_with ~prefix:"FE" key_hex || String.starts_with ~prefix:"FF" key_hex then
              Log.info ("[SKIPLIST-ITER] Stopped after node: " ^ String.sub key_hex 0 16 ^ "... (forward[0]=None)"))
    | Some n ->
        let key_hex = Data.Base16.encode_bytes n.key in
        
        (* Skip only the header sentinel (all 0xFF bytes), not legitimate keys starting with 0xFF *)
        let is_header = Bytes.length n.key = 41 && Bytes.compare n.key t.header.key = 0 in
        
        (* DEBUG: Log nodes near FE *)
        if String.starts_with ~prefix:"FD" key_hex || String.starts_with ~prefix:"FE" key_hex || String.starts_with ~prefix:"FF" key_hex then
          Log.info ("[SKIPLIST-ITER] Visiting node: " ^ String.sub key_hex 0 16 ^ "... is_header=" ^ string_of_bool is_header);
        
        if not is_header then begin
          count := !count + 1;
          last_key := Some n.key;
          if String.starts_with ~prefix:"FE8D5D99" key_hex then begin
            found_fe := true;
            Log.info ("[SKIPLIST-ITER] Found FE8D5D99 during iteration")
          end;
          f ~key:n.key ~value:n.value
        end;
        (* Always continue to next node *)
        visit (Array.get n.forward 0)
  in
  visit (Array.get t.header.forward 0);
  
  (* DEBUG: Log iteration summary *)
  if !count > 0 then
    Log.info ("[SKIPLIST-ITER] Iterated " ^ string_of_int !count ^ " nodes, found_FE=" ^ string_of_bool !found_fe)

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
