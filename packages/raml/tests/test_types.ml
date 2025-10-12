open Std

let test_create_context () =
  let ctx = Raml.Types.create_context () in
  assert (ctx.type_id_counter = 0);
  assert (ctx.type_level = 0)

let test_newvar () =
  let ctx = Raml.Types.create_context () in
  let var1, ctx = Raml.Types.newvar ~ctx (Some "a") in
  let var2, ctx = Raml.Types.newvar ~ctx (Some "b") in

  assert (var1.id = 0);
  assert (var2.id = 1);
  assert (ctx.type_id_counter = 2);

  match var1.desc with
  | Variable (Some "a") -> ()
  | _ -> failwith "Expected Variable with name 'a'"

let test_newty () =
  let ctx = Raml.Types.create_context () in
  let ty, ctx = Raml.Types.newty ~ctx (Variable None) in

  assert (ty.id = 0);
  assert (ty.level = 0);
  assert (ctx.type_id_counter = 1)

let test_no_global_state () =
  let ctx1 = Raml.Types.create_context () in
  let ctx2 = Raml.Types.create_context () in

  let var1, _ctx1 = Raml.Types.newvar ~ctx:ctx1 (Some "a") in
  let var2, _ctx2 = Raml.Types.newvar ~ctx:ctx2 (Some "a") in

  assert (var1.id = 0);
  assert (var2.id = 0)

let test_type_expr_to_string () =
  let ctx = Raml.Types.create_context () in

  let var, _ctx = Raml.Types.newvar ~ctx (Some "a") in
  assert (Raml.Types.type_expr_to_string var = "'a");

  let anon_var, _ctx = Raml.Types.newvar ~ctx None in
  assert (Raml.Types.type_expr_to_string anon_var = "_")

let () =
  test_create_context ();
  test_newvar ();
  test_newty ();
  test_no_global_state ();
  test_type_expr_to_string ();
  Log.info "All types tests passed!"
