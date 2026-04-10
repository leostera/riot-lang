type t =
  | Null
  | Int of int
  | Array of t list

type 'a option =
  | None
  | Some of 'a

let rec diff_at_path path a b =
  match (a, b) with
  | Null, Null -> []
  | Int x, Int y when x = y -> []
  | Array xs, Array ys -> diff_arrays path xs ys
  | _ -> [ path ]

and diff_arrays path xs ys =
  let max_len = max (List.length xs) (List.length ys) in
  let rec loop acc idx =
    if idx >= max_len then
      List.rev acc
    else
      let x_opt =
        try Some (List.nth xs idx) with
        | _ -> None
      in
      let y_opt =
        try Some (List.nth ys idx) with
        | _ -> None
      in
      match (x_opt, y_opt) with
      | Some x, Some y ->
          let diffs = diff_at_path path x y in
          loop (List.rev_append diffs acc) (idx + 1)
      | Some _, None ->
          let diff = path in
          loop (diff :: acc) (idx + 1)
      | None, Some _ ->
          let diff = path in
          loop (diff :: acc) (idx + 1)
      | None, None ->
          loop acc (idx + 1)
  in
  loop [] 0
