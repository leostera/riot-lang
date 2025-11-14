(** Tests for transitive queries *)

open Std
open Poneglyph

let test_simple_transitive () =
  let graph = create () in

  (* Create chain: A -> B -> C *)
  let entity_a = Uri.of_string "test:node:A" in
  let entity_b = Uri.of_string "test:node:B" in
  let entity_c = Uri.of_string "test:node:C" in
  let edge = Uri.of_string "test:next" in
  let source = Uri.of_string "test:source:transitive-test" in

  let facts =
    [
      Fact.make ~source ~entity:entity_a ~attribute:edge ~value:(Fact.Uri entity_b)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~source ~entity:entity_b ~attribute:edge ~value:(Fact.Uri entity_c)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in

  let _ = state graph facts in

  (* Follow transitively from A *)
  let reachable = transitive graph ~start:entity_a ~edge ~max_depth:None in

  (* Should include A, B, and C *)
  if List.length reachable != 3 then
    Error ("Expected 3 reachable nodes, got " ^ string_of_int (List.length reachable))
  else if not (List.mem entity_a reachable) then
    Error "Should include entity A"
  else if not (List.mem entity_b reachable) then
    Error "Should include entity B"
  else if not (List.mem entity_c reachable) then
    Error "Should include entity C"
  else
    Ok ()

let test_transitive_with_depth_limit () =
  let graph = create () in

  (* Create chain: A -> B -> C -> D *)
  let entity_a = Uri.of_string "test:node:A" in
  let entity_b = Uri.of_string "test:node:B" in
  let entity_c = Uri.of_string "test:node:C" in
  let entity_d = Uri.of_string "test:node:D" in
  let edge = Uri.of_string "test:link" in
  let source = Uri.of_string "test:source:depth-test" in

  let facts =
    [
      Fact.make ~source ~entity:entity_a ~attribute:edge ~value:(Fact.Uri entity_b)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~source ~entity:entity_b ~attribute:edge ~value:(Fact.Uri entity_c)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~source ~entity:entity_c ~attribute:edge ~value:(Fact.Uri entity_d)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in

  let _ = state graph facts in

  (* Depth 0 - just the start node *)
  let depth0 = transitive graph ~start:entity_a ~edge ~max_depth:(Some 0) in
  if List.length depth0 != 1 then
    Error "Depth 0 should return 1 node"
  else if not (List.mem entity_a depth0) then
    Error "Depth 0 should include start node"
  else
    (* Depth 1 - start + immediate neighbors *)
    let depth1 = transitive graph ~start:entity_a ~edge ~max_depth:(Some 1) in
    if List.length depth1 != 2 then
      Error "Depth 1 should return 2 nodes"
    else if not (List.mem entity_a depth1 && List.mem entity_b depth1) then
      Error "Depth 1 should include A and B"
    else
      (* Depth 2 *)
      let depth2 = transitive graph ~start:entity_a ~edge ~max_depth:(Some 2) in
      if List.length depth2 != 3 then
        Error "Depth 2 should return 3 nodes"
      else
        (* Unlimited depth *)
        let unlimited = transitive graph ~start:entity_a ~edge ~max_depth:None in
        if List.length unlimited != 4 then
          Error "Unlimited depth should return 4 nodes"
        else
          Ok ()

let test_transitive_with_diamond () =
  let graph = create () in

  (* Create diamond: A -> B -> D, A -> C -> D *)
  let entity_a = Uri.of_string "test:diamond:A" in
  let entity_b = Uri.of_string "test:diamond:B" in
  let entity_c = Uri.of_string "test:diamond:C" in
  let entity_d = Uri.of_string "test:diamond:D" in
  let edge = Uri.of_string "test:depends" in
  let source = Uri.of_string "test:source:diamond-test" in

  let facts =
    [
      Fact.make ~source ~entity:entity_a ~attribute:edge ~value:(Fact.Uri entity_b)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~source ~entity:entity_a ~attribute:edge ~value:(Fact.Uri entity_c)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~source ~entity:entity_b ~attribute:edge ~value:(Fact.Uri entity_d)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~source ~entity:entity_c ~attribute:edge ~value:(Fact.Uri entity_d)
        ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in

  let _ = state graph facts in

  let reachable = transitive graph ~start:entity_a ~edge ~max_depth:None in

  (* Should include all 4 nodes, D only once *)
  if List.length reachable != 4 then
    Error ("Expected 4 nodes in diamond, got " ^ string_of_int (List.length reachable))
  else if not (List.mem entity_a reachable) then
    Error "Should include entity A"
  else if not (List.mem entity_b reachable) then
    Error "Should include entity B"
  else if not (List.mem entity_c reachable) then
    Error "Should include entity C"
  else if not (List.mem entity_d reachable) then
    Error "Should include entity D"
  else
    (* Check no duplicates *)
    let unique = List.sort_uniq Uri.compare reachable in
    if List.length unique != List.length reachable then
      Error "Diamond should not create duplicate nodes"
    else
      Ok ()

let tests =
  Test.[
    case "Simple transitive" test_simple_transitive;
    case "Depth limits" test_transitive_with_depth_limit;
    case "Diamond pattern" test_transitive_with_diamond;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"poneglyph/transitive" ~tests ~args)
    ~args:Env.args ()
