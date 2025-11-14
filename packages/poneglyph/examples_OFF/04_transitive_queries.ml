(** Example 4: Transitive Queries - Following relationships through the graph *)

open Std
open Poneglyph

let () =
  Log.info "=== Example 4: Transitive Queries ===";

  let graph = create () in

  (* Create a dependency graph: A -> B -> C -> D *)
  let module_a = Uri.of_string "example:module:A" in
  let module_b = Uri.of_string "example:module:B" in
  let module_c = Uri.of_string "example:module:C" in
  let module_d = Uri.of_string "example:module:D" in
  let module_e = Uri.of_string "example:module:E" in

  let depends_on_attr = Uri.of_string "example:depends_on" in
  let name_attr = Uri.of_string "example:name" in

  (* Create a more complex graph:
       A -> B -> D
       A -> C -> D
       E (standalone) *)
  let facts =
    [
      (* Names *)
      Fact.make ~entity:module_a ~attribute:name_attr
        ~value:(Fact.String "Module A") ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~entity:module_b ~attribute:name_attr
        ~value:(Fact.String "Module B") ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~entity:module_c ~attribute:name_attr
        ~value:(Fact.String "Module C") ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~entity:module_d ~attribute:name_attr
        ~value:(Fact.String "Module D") ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~entity:module_e ~attribute:name_attr
        ~value:(Fact.String "Module E") ~stated_at:(Datetime.now ()) ~tx_id:0;
      (* Dependencies *)
      Fact.make ~entity:module_a ~attribute:depends_on_attr
        ~value:(Fact.Uri module_b) ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~entity:module_a ~attribute:depends_on_attr
        ~value:(Fact.Uri module_c) ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~entity:module_b ~attribute:depends_on_attr
        ~value:(Fact.Uri module_d) ~stated_at:(Datetime.now ()) ~tx_id:0;
      Fact.make ~entity:module_c ~attribute:depends_on_attr
        ~value:(Fact.Uri module_d) ~stated_at:(Datetime.now ()) ~tx_id:0;
    ]
  in

  let _ = state graph facts in
  Log.info "Created dependency graph";

  (* Find all transitive dependencies from A *)
  Log.info "";
  Log.info "Finding all transitive dependencies from Module A:";
  let deps = transitive graph ~start:module_a ~edge:depends_on_attr ~max_depth:None in
  List.iter
    (fun dep_uri ->
      match get graph ~entity:dep_uri ~attr:name_attr with
      | Some (Fact.String name) -> Log.info ("  - " ^ name)
      | _ -> Log.info ("  - " ^ Uri.to_string dep_uri))
    deps;

  Log.info "";
  Log.info ("Total transitive dependencies: " ^ string_of_int (List.length deps));

  (* Find dependencies with depth limit *)
  Log.info "";
  Log.info "Finding dependencies from Module A (max depth 1):";
  let shallow_deps =
    transitive graph ~start:module_a ~edge:depends_on_attr ~max_depth:(Some 1)
  in
  Log.info ("Found " ^ string_of_int (List.length shallow_deps) ^ " dependencies at depth 1");

  (* Find dependencies from B *)
  Log.info "";
  Log.info "Finding all dependencies from Module B:";
  let b_deps = transitive graph ~start:module_b ~edge:depends_on_attr ~max_depth:None in
  List.iter
    (fun dep_uri ->
      match get graph ~entity:dep_uri ~attr:name_attr with
      | Some (Fact.String name) -> Log.info ("  - " ^ name)
      | _ -> ())
    b_deps;

  (* Module E has no dependencies *)
  Log.info "";
  Log.info "Finding dependencies from Module E (standalone):";
  let e_deps = transitive graph ~start:module_e ~edge:depends_on_attr ~max_depth:None in
  Log.info ("Found " ^ string_of_int (List.length e_deps) ^ " dependencies (only itself)");

  Log.info "=== Example 4 Complete ==="
