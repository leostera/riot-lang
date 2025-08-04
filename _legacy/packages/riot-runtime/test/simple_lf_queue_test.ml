(* Simple standalone test for lf_queue - minimal dependencies *)

let test_basic_operations () =
  let module Lf_queue = struct
    (* Simple test implementation to verify the interface *)
    exception Closed
    
    module Node = struct
      type 'a t = { next : 'a opt Atomic.t; mutable value : 'a }
      and +'a opt
      
      let none : 'a opt = Obj.magic 0
      let closed : 'a opt = Obj.magic 1 
      let some (t : 'a t) : 'a opt = Obj.magic t
      
      let fold (opt : 'a opt) ~none:n ~some =
        if opt == none then n ()
        else if opt == closed then raise Closed
        else some (Obj.magic opt : 'a t)
      
      let make ~next value = { value; next = Atomic.make next }
    end
    
    type 'a t = { tail : 'a Node.t Atomic.t; mutable head : 'a Node.t }
    
    let create () =
      let dummy = { Node.value = Obj.magic (); next = Atomic.make Node.none } in
      { tail = Atomic.make dummy; head = dummy }
    
    let push t x =
      let node = Node.(make ~next:none) x in
      let rec aux () =
        let p = Atomic.get t.tail in
        if Atomic.compare_and_set p.next Node.none (Node.some node) then
          ignore (Atomic.compare_and_set t.tail p node : bool)
        else
          Node.fold (Atomic.get p.next)
            ~none:(fun () -> assert false)
            ~some:(fun p_next ->
              ignore (Atomic.compare_and_set t.tail p p_next : bool);
              aux ())
      in
      aux ()
    
    let pop t =
      let p = t.head in
      let node = Atomic.get p.next in
      Node.fold node
        ~none:(fun () -> None)
        ~some:(fun node ->
          t.head <- node;
          let v = node.value in
          node.value <- Obj.magic ();
          Some v)
    
    let is_empty t =
      Node.fold (Atomic.get t.head.next)
        ~none:(fun () -> true)
        ~some:(fun _ -> false)
  end in
  
  Printf.printf "Testing basic lf_queue operations...\n";
  
  let q = Lf_queue.create () in
  assert (Lf_queue.is_empty q);
  
  Lf_queue.push q 42;
  assert (not (Lf_queue.is_empty q));
  
  let result = Lf_queue.pop q in
  assert (result = Some 42);
  assert (Lf_queue.is_empty q);
  
  Printf.printf "✓ Basic operations test passed\n"

let test_fifo_order () =
  let module Lf_queue = struct
    exception Closed
    
    module Node = struct
      type 'a t = { next : 'a opt Atomic.t; mutable value : 'a }
      and +'a opt
      
      let none : 'a opt = Obj.magic 0
      let closed : 'a opt = Obj.magic 1 
      let some (t : 'a t) : 'a opt = Obj.magic t
      
      let fold (opt : 'a opt) ~none:n ~some =
        if opt == none then n ()
        else if opt == closed then raise Closed
        else some (Obj.magic opt : 'a t)
      
      let make ~next value = { value; next = Atomic.make next }
    end
    
    type 'a t = { tail : 'a Node.t Atomic.t; mutable head : 'a Node.t }
    
    let create () =
      let dummy = { Node.value = Obj.magic (); next = Atomic.make Node.none } in
      { tail = Atomic.make dummy; head = dummy }
    
    let push t x =
      let node = Node.(make ~next:none) x in
      let rec aux () =
        let p = Atomic.get t.tail in
        if Atomic.compare_and_set p.next Node.none (Node.some node) then
          ignore (Atomic.compare_and_set t.tail p node : bool)
        else
          Node.fold (Atomic.get p.next)
            ~none:(fun () -> assert false)
            ~some:(fun p_next ->
              ignore (Atomic.compare_and_set t.tail p p_next : bool);
              aux ())
      in
      aux ()
    
    let pop t =
      let p = t.head in
      let node = Atomic.get p.next in
      Node.fold node
        ~none:(fun () -> None)
        ~some:(fun node ->
          t.head <- node;
          let v = node.value in
          node.value <- Obj.magic ();
          Some v)
  end in
  
  Printf.printf "Testing FIFO order...\n";
  
  let q = Lf_queue.create () in
  let values = [1; 2; 3; 4; 5] in
  List.iter (Lf_queue.push q) values;
  
  let rec pop_all acc =
    match Lf_queue.pop q with
    | None -> List.rev acc
    | Some v -> pop_all (v :: acc)
  in
  let popped = pop_all [] in
  assert (popped = values);
  
  Printf.printf "✓ FIFO order test passed\n"

let run_tests () =
  Printf.printf "Running simple lf_queue tests...\n\n";
  test_basic_operations ();
  test_fifo_order ();
  Printf.printf "\n✅ Simple lf_queue tests completed!\n"

let () = run_tests ()