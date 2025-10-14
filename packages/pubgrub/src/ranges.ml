open Std

type 'v bound = Unbounded | Included of 'v | Excluded of 'v
type 'v range = 'v bound * 'v bound
type 'v t = 'v range list

let empty = []
let full = [ (Unbounded, Unbounded) ]
let singleton v = [ (Included v, Included v) ]
let higher_than v = [ (Included v, Unbounded) ]
let strictly_higher_than v = [ (Excluded v, Unbounded) ]
let lower_than v = [ (Unbounded, Included v) ]
let strictly_lower_than v = [ (Unbounded, Excluded v) ]
let between v1 v2 = [ (Included v1, Excluded v2) ]
let is_empty ranges = ranges = []

let compare_bound ~compare_v b1 b2 =
  match (b1, b2) with
  | Unbounded, Unbounded -> 0
  | Unbounded, _ -> -1
  | _, Unbounded -> 1
  | Included v1, Included v2 -> compare_v v1 v2
  | Included v1, Excluded v2 -> ( match compare_v v1 v2 with 0 -> -1 | n -> n)
  | Excluded v1, Included v2 -> ( match compare_v v1 v2 with 0 -> 1 | n -> n)
  | Excluded v1, Excluded v2 -> compare_v v1 v2

let compare_bound_start = compare_bound

let compare_bound_end ~compare_v b1 b2 =
  match (b1, b2) with
  | Unbounded, Unbounded -> 0
  | Unbounded, _ -> 1
  | _, Unbounded -> -1
  | Included v1, Included v2 -> compare_v v1 v2
  | Included v1, Excluded v2 -> ( match compare_v v1 v2 with 0 -> 1 | n -> n)
  | Excluded v1, Included v2 -> ( match compare_v v1 v2 with 0 -> -1 | n -> n)
  | Excluded v1, Excluded v2 -> compare_v v1 v2

let valid_segment ~compare_v (start, end_) =
  match (start, end_) with
  | Included v1, Included v2 -> compare_v v1 v2 <= 0
  | Included v1, Excluded v2 -> compare_v v1 v2 < 0
  | Excluded v1, Included v2 -> compare_v v1 v2 < 0
  | Excluded v1, Excluded v2 -> compare_v v1 v2 < 0
  | Unbounded, _ -> true
  | _, Unbounded -> true

let negate_bound bound =
  match bound with
  | Included v -> Excluded v
  | Excluded v -> Included v
  | Unbounded -> Unbounded

let complement ~compare_v ranges =
  match ranges with
  | [] -> full
  | [ (Unbounded, Unbounded) ] -> empty
  | (Included v, Unbounded) :: _ -> [ (Unbounded, Excluded v) ]
  | (Excluded v, Unbounded) :: _ -> [ (Unbounded, Included v) ]
  | segments ->
      let rec negate_segments start acc = function
        | [] ->
            if start = Unbounded then List.rev acc
            else List.rev ((start, Unbounded) :: acc)
        | (v1, v2) :: rest ->
            let new_segment = (start, negate_bound v1) in
            let new_start = negate_bound v2 in
            negate_segments new_start (new_segment :: acc) rest
      in
      let start =
        match segments with
        | (Unbounded, Included v) :: _ -> Excluded v
        | (Unbounded, Excluded v) :: _ -> Included v
        | _ -> Unbounded
      in
      let segments_to_process =
        match segments with (Unbounded, _) :: rest -> rest | _ -> segments
      in
      negate_segments start [] segments_to_process

let within_bounds ~compare_v version (start, end_) =
  let after_start =
    match start with
    | Unbounded -> true
    | Included v -> compare_v version v >= 0
    | Excluded v -> compare_v version v > 0
  in
  let before_end =
    match end_ with
    | Unbounded -> true
    | Included v -> compare_v version v <= 0
    | Excluded v -> compare_v version v < 0
  in
  after_start && before_end

let contains ~compare_v ranges version =
  List.exists (within_bounds ~compare_v version) ranges

let max_by cmp a b = if cmp a b >= 0 then a else b
let min_by cmp a b = if cmp a b <= 0 then a else b

let rec intersection ~compare_v r1 r2 =
  match (r1, r2) with
  | [], _ | _, [] -> []
  | (s1, e1) :: rest1, (s2, e2) :: rest2 ->
      let start = max_by (compare_bound_start ~compare_v) s1 s2 in
      let end_ = min_by (compare_bound_end ~compare_v) e1 e2 in
      if valid_segment ~compare_v (start, end_) then
        (start, end_) :: intersection ~compare_v rest1 rest2
      else
        let cmp_end = compare_bound_end ~compare_v e1 e2 in
        if cmp_end <= 0 then intersection ~compare_v rest1 r2
        else intersection ~compare_v r1 rest2

let union ~compare_v r1 r2 =
  complement ~compare_v
    (intersection ~compare_v (complement ~compare_v r1)
       (complement ~compare_v r2))

let is_disjoint ~compare_v r1 r2 = intersection ~compare_v r1 r2 = empty
let subset_of ~compare_v r1 r2 = intersection ~compare_v r1 r2 = r1
