(* Dense shortest-path dynamic over adjacency lists. *)
let graph =
  [|
    [ (1, 7); (2, 9); (5, 14) ];
    [ (2, 10); (3, 15) ];
    [ (3, 11); (5, 2) ];
    [ (4, 6) ];
    [];
    [ (4, 9) ];
  |]

let dijkstra src =
  let n = Array.length graph in
  let dist = Array.make n max_int in
  let used = Array.make n false in
  dist.(src) <- 0;
  for _ = 0 to n - 1 do
    let best = ref (-1) in
    for v = 0 to n - 1 do
      if (not used.(v))
         && (match !best with
             | -1 -> true
             | b -> dist.(v) < dist.(b))
      then best := v
    done;
    if !best <> -1 then begin
      let v = !best in
      used.(v) <- true;
      List.iter
        (fun (w, cost) ->
          if dist.(v) <> max_int then
            let cand = dist.(v) + cost in
            if cand < dist.(w) then dist.(w) <- cand)
        graph.(v)
    end
  done;
  dist

let () =
  dijkstra 0 |> Array.iter (fun d -> Printf.printf "%d " d);
  print_newline ()
