(* Breadth-first traversal with Queue. *)
let graph =
  [|
    [ 1; 2 ];
    [ 3 ];
    [ 3; 4 ];
    [ 5 ];
    [];
    []
  |]

let bfs start =
  let q = Queue.create () in
  let seen = Array.make (Array.length graph) false in
  let order = ref [] in
  seen.(start) <- true;
  Queue.push start q;
  while not (Queue.is_empty q) do
    let v = Queue.pop q in
    order := v :: !order;
    List.iter
      (fun w ->
        if not seen.(w) then begin
          seen.(w) <- true;
          Queue.push w q
        end)
      graph.(v)
  done;
  List.rev !order

let () =
  bfs 0 |> List.iter (fun v -> Printf.printf "%d " v);
  print_newline ()
