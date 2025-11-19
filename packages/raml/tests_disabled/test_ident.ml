open Std

let test_create_local () =
  let ctx = Raml.Identifier.create_context () in
  let id1, ctx = Raml.Identifier.create_local ~ctx "foo" in
  let id2, ctx = Raml.Identifier.create_local ~ctx "bar" in

  assert (Raml.Identifier.name id1 = "foo");
  assert (Raml.Identifier.name id2 = "bar");
  assert (ctx.stamp_counter = 2)

let test_create_scoped () =
  let ctx = Raml.Identifier.create_context () in
  let id, _ctx = Raml.Identifier.create_scoped ~ctx ~scope:100 "test" in

  assert (Raml.Identifier.name id = "test");
  assert (Raml.Identifier.scope id = 100)

let test_unique_name () =
  let ctx = Raml.Identifier.create_context () in
  let id1, ctx = Raml.Identifier.create_local ~ctx "x" in
  let id2, _ctx = Raml.Identifier.create_local ~ctx "x" in

  let unique1 = Raml.Identifier.unique_name id1 in
  let unique2 = Raml.Identifier.unique_name id2 in

  assert (unique1 = "x_0");
  assert (unique2 = "x_1")

let test_no_global_state () =
  let ctx1 = Raml.Identifier.create_context () in
  let ctx2 = Raml.Identifier.create_context () in

  let id1, _ctx1 = Raml.Identifier.create_local ~ctx:ctx1 "test" in
  let id2, _ctx2 = Raml.Identifier.create_local ~ctx:ctx2 "test" in

  match (id1, id2) with
  | Local { stamp = s1; _ }, Local { stamp = s2; _ } ->
      assert (s1 = 0);
      assert (s2 = 0)
  | _ -> panic "Expected Local identifiers"

let test_equal () =
  let ctx = Raml.Identifier.create_context () in
  let id1, ctx = Raml.Identifier.create_local ~ctx "x" in
  let id2, _ctx = Raml.Identifier.create_local ~ctx "x" in

  assert (not (Raml.Identifier.equal id1 id2))

let () =
  test_create_local ();
  test_create_scoped ();
  test_unique_name ();
  test_no_global_state ();
  test_equal ();
  Log.info "All identifier tests passed!"
