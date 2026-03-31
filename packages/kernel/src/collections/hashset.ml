open Global0

type 'a t = ('a, unit) Hashtbl.t

let create = fun () -> Hashtbl.create 16

let with_capacity = fun capacity -> Hashtbl.create capacity

let of_list = fun elements ->
    let set = Hashtbl.create (List.length elements) in
    List.iter
      (fun elem ->
        Hashtbl.replace set elem ())
      elements;
    set

let insert = fun set value ->
    let was_present = Hashtbl.mem set value in
    Hashtbl.replace set value ();
    not was_present

let remove = fun set value ->
    let was_present = Hashtbl.mem set value in
    Hashtbl.remove set value;
    was_present

let contains = fun set value ->
    Hashtbl.mem set value

let len = fun set -> Hashtbl.length set

let is_empty = fun set -> Hashtbl.length set = 0

let clear = fun set -> Hashtbl.clear set

let iter = fun set ~fn ->
    Hashtbl.iter (fun elem _ -> fn elem) set

let fold = fun set ~init ~fn ->
    Hashtbl.fold (fun elem _ acc -> fn acc elem) set init

let to_list = fun set ->
    Hashtbl.fold (fun elem _ acc -> elem :: acc) set []

let union = fun set1 set2 ->
    let result = Hashtbl.copy set1 in
    Hashtbl.iter
      (fun elem _ ->
        Hashtbl.replace result elem ())
      set2;
    result

let intersection = fun set1 set2 ->
    let result = Hashtbl.create 16 in
    Hashtbl.iter
      (fun elem _ ->
        if Hashtbl.mem set2 elem then
          Hashtbl.replace result elem ())
      set1;
    result

let difference = fun set1 set2 ->
    let result = Hashtbl.create 16 in
    Hashtbl.iter
      (fun elem _ ->
        if not (Hashtbl.mem set2 elem) then
          Hashtbl.replace result elem ())
      set1;
    result

let symmetric_difference = fun set1 set2 ->
    let result = Hashtbl.create 16 in
    Hashtbl.iter
      (fun elem _ ->
        if not (Hashtbl.mem set2 elem) then
          Hashtbl.replace result elem ())
      set1;
    Hashtbl.iter
      (fun elem _ ->
        if not (Hashtbl.mem set1 elem) then
          Hashtbl.replace result elem ())
      set2;
    result

let is_subset = fun set1 set2 ->
    Hashtbl.fold (fun elem _ acc -> acc && Hashtbl.mem set2 elem) set1 true

let is_superset = fun set1 set2 -> is_subset set2 set1

let is_disjoint = fun set1 set2 ->
    Hashtbl.fold (fun elem _ acc -> acc && not (Hashtbl.mem set2 elem)) set1 true

let into_iter : type item. item t -> item Iter.Iterator.t = fun set ->
    let module SetIter = struct
      type state = {
        items: item list;
        pos: int;
      }

      type nonrec item = item

      let next = fun state ->
          if state.pos >= List.length state.items then
            (None, state)
          else
            let item = List.nth state.items state.pos in
            (Some item, {state with pos = state.pos + 1})

      let size = fun state -> max 0 (List.length state.items - state.pos)
    end in
    let items = to_list set in
    Iter.Iterator.make (module SetIter) {SetIter.items; pos = 0}

let to_mut_iter : type item. item t -> item Iter.MutIterator.t = fun set ->
    let module SetIter = struct
      type state = {
        items: item list;
        mutable pos: int;
      }

      type nonrec item = item

      let next = fun state ->
          if state.pos >= List.length state.items then
            None
          else
            let item = List.nth state.items state.pos in
            state.pos <- state.pos + 1;
            Some item

      let size = fun state -> max 0 (List.length state.items - state.pos)

      let clone = fun state -> {items = state.items; pos = state.pos}
    end in
    let items = to_list set in
    Iter.MutIterator.make (module SetIter) {SetIter.items; pos = 0}
