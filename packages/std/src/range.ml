open Global
open Kernel

type 'a bound =
  | Included of 'a
  | Excluded of 'a
  | Unbounded

type 'a t = { lower: 'a bound; upper: 'a bound; compare: 'a -> 'a -> Order.t }

let make = fun ~lower ~upper ~compare -> { lower; upper; compare }

let all = fun ~compare -> make ~lower:Unbounded ~upper:Unbounded ~compare

let singleton = fun ~compare value -> make ~lower:(Included value) ~upper:(Included value) ~compare

let closed = fun ~compare lower upper -> make ~lower:(Included lower) ~upper:(Included upper) ~compare

let open_ = fun ~compare lower upper -> make ~lower:(Excluded lower) ~upper:(Excluded upper) ~compare

let closed_open = fun ~compare lower upper -> make ~lower:(Included lower) ~upper:(Excluded upper) ~compare

let open_closed = fun ~compare lower upper -> make ~lower:(Excluded lower) ~upper:(Included upper) ~compare

let at_least = fun ~compare lower -> make ~lower:(Included lower) ~upper:Unbounded ~compare

let greater_than = fun ~compare lower -> make ~lower:(Excluded lower) ~upper:Unbounded ~compare

let at_most = fun ~compare upper -> make ~lower:Unbounded ~upper:(Included upper) ~compare

let less_than = fun ~compare upper -> make ~lower:Unbounded ~upper:(Excluded upper) ~compare

let lower_bound = fun t -> t.lower

let upper_bound = fun t -> t.upper

let compare_values = fun t -> t.compare

let compare_lower_bounds = fun t left right ->
  match left, right with
  | Unbounded, Unbounded -> Order.EQ
  | Unbounded, _ -> Order.LT
  | _, Unbounded -> Order.GT
  | (Included left, Included right) | (Excluded left, Excluded right) -> t.compare left right
  | Included left, Excluded right ->
      let order = t.compare left right in
      if order = Order.EQ then
        Order.LT
      else order
  | Excluded left, Included right ->
      let order = t.compare left right in
      if order = Order.EQ then
        Order.GT
      else order

let compare_upper_bounds = fun t left right ->
  match left, right with
  | Unbounded, Unbounded -> Order.EQ
  | Unbounded, _ -> Order.GT
  | _, Unbounded -> Order.LT
  | (Included left, Included right) | (Excluded left, Excluded right) -> t.compare left right
  | Included left, Excluded right ->
      let order = t.compare left right in
      if order = Order.EQ then
        Order.GT
      else order
  | Excluded left, Included right ->
      let order = t.compare left right in
      if order = Order.EQ then
        Order.LT
      else order

let max_lower_bound = fun t left right ->
  if compare_lower_bounds t left right != Order.LT then
    left
  else right

let min_lower_bound = fun t left right ->
  if compare_lower_bounds t left right != Order.GT then
    left
  else right

let min_upper_bound = fun t left right ->
  if compare_upper_bounds t left right != Order.GT then
    left
  else right

let max_upper_bound = fun t left right ->
  if compare_upper_bounds t left right != Order.LT then
    left
  else right

let contains = fun t value ->
  let above_lower =
    match t.lower with
    | Unbounded -> true
    | Included lower -> t.compare value lower != Order.LT
    | Excluded lower -> t.compare value lower = Order.GT
  in
  let below_upper =
    match t.upper with
    | Unbounded -> true
    | Included upper -> t.compare value upper != Order.GT
    | Excluded upper -> t.compare value upper = Order.LT
  in
  above_lower && below_upper

let is_empty = fun t ->
  match t.lower, t.upper with
  | (Unbounded, _) | (_, Unbounded) -> false
  | Included lower, Included upper -> t.compare lower upper = Order.GT
  | (Included lower, Excluded upper) | (Excluded lower, Included upper) | (Excluded lower, Excluded upper) ->
      let order = t.compare lower upper in
      if order = Order.GT then
        true
      else
        if order = Order.LT then
          false
        else
          (
            match t.lower, t.upper with
            | Included _, Included _ -> false
            | _ -> true
          )

let intersect = fun left right ->
  let range = { lower = max_lower_bound left left.lower right.lower; upper = min_upper_bound left left.upper right.upper; compare = left.compare } in
  if is_empty range then
    None
  else Some range

let overlaps = fun left right ->
  match intersect left right with
  | Some _ -> true
  | None -> false

let hull = fun left right ->
  if is_empty left then
    right
  else
    if is_empty right then
      left
    else { lower = min_lower_bound left left.lower right.lower; upper = max_upper_bound left left.upper right.upper; compare = left.compare }

let to_string = fun render t ->
  match t.lower, t.upper with
  | Unbounded, Unbounded -> "(..)"
  | _ ->
      let left_delim =
        match t.lower with
        | Included _ -> "["
        | Excluded _ | Unbounded -> "("
      in
      let right_delim =
        match t.upper with
        | Included _ -> "]"
        | Excluded _ | Unbounded -> ")"
      in
      let lower =
        match t.lower with
        | Included value | Excluded value -> render value
        | Unbounded -> ".."
      in
      let upper =
        match t.upper with
        | Included value | Excluded value -> render value
        | Unbounded -> ".."
      in
      left_delim ^ lower ^ "," ^ upper ^ right_delim
