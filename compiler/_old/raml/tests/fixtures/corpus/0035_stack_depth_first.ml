(* Depth-first traversal with Stack. *)
let graph =
  [|
    [ 1; 2 ];
    [ 3 ];
    [ 3; 4 ];
    [ 5 ];
    [];
    []
  |]

let dfs start =
  let st = Stack.create () in
  let seen = Array.make (Array.length graph) false in
  let order = ref [] in
  Stack.push start st;
  while not (Stack.is_empty st) do
    let v = Stack.pop st in
    if not seen.(v) then begin
      seen.(v) <- true;
      order := v :: !order;
      List.iter (fun w -> Stack.push w st) (List.rev graph.(v))
    end
  done;
  List.rev !order

let () =
  dfs 0 |> List.iter (fun v -> Printf.printf "%d " v);
  print_newline ()
