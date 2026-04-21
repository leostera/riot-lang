open Std

let test_closed_range_contains_endpoints = fun _ctx ->
  let range = Range.closed ~compare:Int.compare 1 5 in
  if not (Range.contains range 1) then
    Error "Closed ranges should include their lower endpoint"
  else if not (Range.contains range 5) then
    Error "Closed ranges should include their upper endpoint"
  else if not (Range.contains range 3) then
    Error "Closed ranges should include interior points"
  else
    Ok ()

let test_open_range_excludes_endpoints = fun _ctx ->
  let range = Range.open_ ~compare:Int.compare 1 5 in
  if Range.contains range 1 then
    Error "Open ranges should exclude their lower endpoint"
  else if Range.contains range 5 then
    Error "Open ranges should exclude their upper endpoint"
  else if not (Range.contains range 3) then
    Error "Open ranges should still include interior points"
  else
    Ok ()

let test_mixed_bounds_respect_their_edge = fun _ctx ->
  let left_closed = Range.closed_open ~compare:Int.compare 1 5 in
  let right_closed = Range.open_closed ~compare:Int.compare 1 5 in
  if not (Range.contains left_closed 1) || Range.contains left_closed 5 then
    Error "Closed-open ranges should include only the lower endpoint"
  else if Range.contains right_closed 1 || not (Range.contains right_closed 5) then
    Error "Open-closed ranges should include only the upper endpoint"
  else
    Ok ()

let test_empty_detection_tracks_open_closed_semantics = fun _ctx ->
  let closed = Range.closed ~compare:Int.compare 1 1 in
  let open_ = Range.open_ ~compare:Int.compare 1 1 in
  let half_open = Range.closed_open ~compare:Int.compare 1 1 in
  if Range.is_empty closed then
    Error "[1,1] should not be empty"
  else if not (Range.is_empty open_) then
    Error "(1,1) should be empty"
  else if not (Range.is_empty half_open) then
    Error "[1,1) should be empty"
  else
    Ok ()

let test_one_sided_ranges = fun _ctx ->
  let lower = Range.at_least ~compare:Int.compare 3 in
  let upper = Range.less_than ~compare:Int.compare 7 in
  if not (Range.contains lower 3) || not (Range.contains lower 99) then
    Error "Ranges with only a lower bound should include all later values"
  else if Range.contains lower 2 then
    Error "Ranges with only a lower bound should exclude earlier values"
  else if not (Range.contains upper 6) then
    Error "Ranges with only an upper bound should include earlier values"
  else if Range.contains upper 7 then
    Error "Exclusive upper bounds should exclude the endpoint"
  else
    Ok ()

let test_intersection_respects_endpoint_inclusion = fun _ctx ->
  let left = Range.closed ~compare:Int.compare 1 5 in
  let touching_open = Range.open_closed ~compare:Int.compare 5 8 in
  let touching_closed = Range.closed ~compare:Int.compare 5 8 in
  match (Range.intersect left touching_open, Range.intersect left touching_closed) with
  | None, Some intersection when Range.contains intersection 5 && not (Range.contains intersection 4) -> Ok ()
  | Some _, _ -> Error "Intersection should be empty when the touching endpoint is excluded"
  | None, None -> Error "Closed ranges that touch at the endpoint should intersect"
  | None, Some _ -> Error "Closed touching ranges should intersect at the shared point"

let test_hull_covers_both_ranges = fun _ctx ->
  let left = Range.open_closed ~compare:Int.compare 3 6 in
  let right = Range.closed_open ~compare:Int.compare 1 4 in
  let hull = Range.hull left right in
  if not (String.equal (Range.to_string Int.to_string hull) "[1,6]") then
    Error "Range.hull should choose the weakest lower and upper bounds"
  else
    Ok ()

let test_range_uses_stored_compare = fun _ctx ->
  let descending left right = Int.compare right left in
  let range = Range.closed ~compare:descending 5 1 in
  if not (Range.contains range 3) then
    Error "Range.contains should respect the comparator captured at construction"
  else if Range.contains range 0 then
    Error "Descending ranges should still exclude values outside their ordering bounds"
  else if not (String.equal (Range.to_string Int.to_string range) "[5,1]") then
    Error "Range.to_string should preserve stored endpoints"
  else
    Ok ()

let tests =
  Test.[
    case "closed ranges include both endpoints" test_closed_range_contains_endpoints;
    case "open ranges exclude both endpoints" test_open_range_excludes_endpoints;
    case "mixed bounds respect the open and closed edge" test_mixed_bounds_respect_their_edge;
    case "is_empty tracks open and closed semantics" test_empty_detection_tracks_open_closed_semantics;
    case "one-sided ranges honor their only bound" test_one_sided_ranges;
    case "intersect respects endpoint inclusion" test_intersection_respects_endpoint_inclusion;
    case "hull covers both ranges" test_hull_covers_both_ranges;
    case "stored compare controls membership" test_range_uses_stored_compare;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"range" ~tests ~args ()) ~args:Env.args ()
