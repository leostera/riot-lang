open Std

type 'v bound =
  | Unbounded
  | Included of 'v
  | Excluded of 'v

type 'v range = 'v bound * 'v bound

type 'v t = 'v range list

let empty = []

let full = [ (Unbounded, Unbounded); ]

let singleton = fun v -> [ (Included v, Included v); ]

let higher_than = fun v -> [ (Included v, Unbounded); ]

let strictly_higher_than = fun v -> [ (Excluded v, Unbounded); ]

let lower_than = fun v -> [ (Unbounded, Included v); ]

let strictly_lower_than = fun v -> [ (Unbounded, Excluded v); ]

let between = fun v1 v2 -> [ (Included v1, Excluded v2); ]

let is_empty = fun ranges -> ranges = []

let segments = fun ranges -> ranges

let compare_bound = fun ~compare_v b1 b2 ->
  match (b1, b2) with
  | (Unbounded, Unbounded) -> Order.EQ
  | (Unbounded, _) -> Order.LT
  | (_, Unbounded) -> Order.GT
  | (Included v1, Included v2) -> compare_v v1 v2
  | (Included v1, Excluded v2) -> (
      match compare_v v1 v2 with
      | Order.EQ -> Order.LT
      | n -> n
    )
  | (Excluded v1, Included v2) -> (
      match compare_v v1 v2 with
      | Order.EQ -> Order.GT
      | n -> n
    )
  | (Excluded v1, Excluded v2) -> compare_v v1 v2

let compare_bound_start = compare_bound

let compare_bound_end = fun ~compare_v b1 b2 ->
  match (b1, b2) with
  | (Unbounded, Unbounded) -> Order.EQ
  | (Unbounded, _) -> Order.GT
  | (_, Unbounded) -> Order.LT
  | (Included v1, Included v2) -> compare_v v1 v2
  | (Included v1, Excluded v2) -> (
      match compare_v v1 v2 with
      | Order.EQ -> Order.GT
      | n -> n
    )
  | (Excluded v1, Included v2) -> (
      match compare_v v1 v2 with
      | Order.EQ -> Order.LT
      | n -> n
    )
  | (Excluded v1, Excluded v2) -> compare_v v1 v2

let max_by = fun cmp a b ->
  match cmp a b with
  | Order.LT -> b
  | Order.EQ
  | Order.GT -> a

let min_by = fun cmp a b ->
  match cmp a b with
  | Order.LT
  | Order.EQ -> a
  | Order.GT -> b

let valid_segment = fun ~compare_v ((start, end_)) ->
  match (start, end_) with
  | (Included v1, Included v2) -> (
      match compare_v v1 v2 with
      | Order.LT
      | Order.EQ -> true
      | Order.GT -> false
    )
  | (Included v1, Excluded v2)
  | (Excluded v1, Included v2)
  | (Excluded v1, Excluded v2) -> (
      match compare_v v1 v2 with
      | Order.LT -> true
      | Order.EQ
      | Order.GT -> false
    )
  | (Unbounded, _) -> true
  | (_, Unbounded) -> true

let add_segment_if_valid = fun ~compare_v acc segment ->
  if valid_segment ~compare_v segment then
    segment :: acc
  else
    acc

let end_before_start = fun ~compare_v end_ start ->
  match (end_, start) with
  | (Unbounded, _)
  | (_, Unbounded) -> false
  | (Included left, Included right)
  | (Included left, Excluded right)
  | (Excluded left, Included right)
  | (Excluded left, Excluded right) -> (
      match compare_v left right with
      | Order.LT -> true
      | Order.GT -> false
      | Order.EQ ->
          match (end_, start) with
          | (Excluded _, Excluded _) -> true
          | _ -> false
    )

let normalize = fun ~compare_v ranges ->
  let sorted =
    List.sort
      (List.filter ranges ~fn:(valid_segment ~compare_v))
      ~compare:(fun (left_start, left_end) (right_start, right_end) ->
        match compare_bound_start ~compare_v left_start right_start with
        | Order.EQ -> compare_bound_end ~compare_v left_end right_end
        | n -> n)
  in
  let rec merge acc = function
    | [] ->
        List.reverse acc
    | segment :: rest -> (
        match acc with
        | [] -> merge [ segment ] rest
        | (current_start, current_end) :: acc_rest ->
            let (next_start, next_end) = segment in
            if end_before_start ~compare_v current_end next_start then
              merge (segment :: acc) rest
            else
              let merged_end = max_by (compare_bound_end ~compare_v) current_end next_end in
              merge ((current_start, merged_end) :: acc_rest) rest
      )
  in
  merge [] sorted

let negate_bound = fun bound ->
  match bound with
  | Included v -> Excluded v
  | Excluded v -> Included v
  | Unbounded -> Unbounded

let complement = fun ~compare_v ranges ->
  let ranges = normalize ~compare_v ranges in
  if ranges = [] then
    full
  else
    let rec build current_start acc = function
      | [] ->
          List.reverse
            (
              match current_start with
              | Unbounded -> acc
              | _ -> (current_start, Unbounded) :: acc
            )
      | (start, end_) :: rest ->
          let acc =
            match start with
            | Unbounded -> acc
            | _ -> add_segment_if_valid ~compare_v acc (current_start, negate_bound start)
          in
          build (negate_bound end_) acc rest
    in
    build Unbounded [] ranges

let within_bounds = fun ~compare_v version ((start, end_)) ->
  let after_start =
    match start with
    | Unbounded -> true
    | Included v -> (
        match compare_v version v with
        | Order.LT -> false
        | Order.EQ
        | Order.GT -> true
      )
    | Excluded v -> (
        match compare_v version v with
        | Order.GT -> true
        | Order.LT
        | Order.EQ -> false
      )
  in
  let before_end =
    match end_ with
    | Unbounded -> true
    | Included v -> (
        match compare_v version v with
        | Order.LT
        | Order.EQ -> true
        | Order.GT -> false
      )
    | Excluded v -> (
        match compare_v version v with
        | Order.LT -> true
        | Order.EQ
        | Order.GT -> false
      )
  in
  after_start && before_end

let contains = fun ~compare_v ranges version ->
  List.any
    ranges
    ~fn:(within_bounds ~compare_v version)

let rec intersection = fun ~compare_v r1 r2 ->
  let r1 = normalize ~compare_v r1 in
  let r2 = normalize ~compare_v r2 in
  let rec compute left right =
    match (left, right) with
    | ([], _)
    | (_, []) -> []
    | ((s1, e1) :: rest1, (s2, e2) :: rest2) ->
        let start = max_by (compare_bound_start ~compare_v) s1 s2 in
        let end_ = min_by (compare_bound_end ~compare_v) e1 e2 in
        let tail =
          let cmp_end = compare_bound_end ~compare_v e1 e2 in
          match cmp_end with
          | Order.LT -> compute rest1 right
          | Order.GT -> compute left rest2
          | Order.EQ -> compute rest1 rest2
        in
        if valid_segment ~compare_v (start, end_) then
          (start, end_) :: tail
        else
          tail
  in
  normalize ~compare_v (compute r1 r2)

let union = fun ~compare_v r1 r2 -> normalize ~compare_v (segments r1 @ segments r2)

let is_disjoint = fun ~compare_v r1 r2 -> is_empty (intersection ~compare_v r1 r2)

let subset_of = fun ~compare_v r1 r2 ->
  normalize ~compare_v (intersection ~compare_v r1 r2) = normalize ~compare_v r1

let compare = fun ~compare_v left right ->
  let rec compare_segments left right =
    match (left, right) with
    | ([], []) -> Order.EQ
    | ([], _) -> Order.LT
    | (_, []) -> Order.GT
    | ((left_start, left_end) :: left_rest, (right_start, right_end) :: right_rest) -> (
        match compare_bound_start ~compare_v left_start right_start with
        | Order.EQ -> (
            match compare_bound_end ~compare_v left_end right_end with
            | Order.EQ -> compare_segments left_rest right_rest
            | n -> n
          )
        | n -> n
      )
  in
  compare_segments (normalize ~compare_v left) (normalize ~compare_v right)

let equal = fun ~compare_v left right ->
  match compare ~compare_v left right with
  | Order.EQ -> true
  | Order.LT
  | Order.GT -> false

let bound_to_string = fun ~to_string_v bound ->
  match bound with
  | Unbounded -> ""
  | Included value
  | Excluded value -> to_string_v value

let range_to_string = fun ~to_string_v (start, end_) ->
  let left_bracket =
    match start with
    | Included _ -> "["
    | Excluded _
    | Unbounded -> "("
  in
  let right_bracket =
    match end_ with
    | Included _ -> "]"
    | Excluded _
    | Unbounded -> ")"
  in
  let start_text =
    match start with
    | Unbounded -> "-inf"
    | _ -> bound_to_string ~to_string_v start
  in
  let end_text =
    match end_ with
    | Unbounded -> "+inf"
    | _ -> bound_to_string ~to_string_v end_
  in
  left_bracket ^ start_text ^ ", " ^ end_text ^ right_bracket

let to_string = fun ~to_string_v ranges ->
  match ranges with
  | [] -> "empty"
  | [ (Unbounded, Unbounded) ] -> "*"
  | _ -> String.concat " | " (List.map ranges ~fn:(range_to_string ~to_string_v))
