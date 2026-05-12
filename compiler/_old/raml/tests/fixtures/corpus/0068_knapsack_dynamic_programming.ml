(* Table-based dynamic programming. *)
let items =
  [|
    (2, 3);
    (3, 4);
    (4, 5);
    (5, 8);
  |]

let capacity = 8

let best_value () =
  let n = Array.length items in
  let dp = Array.make_matrix (n + 1) (capacity + 1) 0 in
  for i = 1 to n do
    let weight, value = items.(i - 1) in
    for w = 0 to capacity do
      dp.(i).(w) <- dp.(i - 1).(w);
      if weight <= w then
        dp.(i).(w) <-
          max dp.(i).(w) (dp.(i - 1).(w - weight) + value)
    done
  done;
  dp.(n).(capacity)

let () = Printf.printf "%d\n" (best_value ())
