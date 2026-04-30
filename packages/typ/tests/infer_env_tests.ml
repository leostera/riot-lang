open Std
open Std.Result.Syntax

module Ast = Typ.Ast
module Env = Typ.Infer.Env
module Type = Typ.Ast.Type
module TypeScheme = Typ.Infer.TypeScheme
module SurfacePath = Typ.Model.Surface_path

let ident name =
  Syn.parse_ident name
  |> Option.map ~fn:SurfacePath.from_syn_ident
  |> Option.expect ~msg:("expected surface path test identifier " ^ name)

let nominal name = Type.Constructor { ident = ident name; arguments = [] }

let int_type = fun () -> nominal "int"

let bool_type = fun () -> nominal "bool"

let scheme type_ = TypeScheme.monomorphic type_

let origin: Ast.origin = {
  span = Syn.Span.make ~start:0 ~end_:0;
  kind = Syn.SyntaxKind.TYPE_DECL;
}

let type_decl name: Ast.type_declaration = {
  Ast.origin;
  name = ident name;
  parameters = [];
  definition = { origin; kind = Ast.Abstract };
}

let assert_scheme_body ~expected actual =
  match actual with
  | Some actual when Type.equal actual.TypeScheme.body expected -> Ok ()
  | Some (actual: TypeScheme.t) ->
      Error ("expected " ^ Type.to_string expected ^ " but found " ^ Type.to_string actual.body)
  | None -> Error ("expected " ^ Type.to_string expected ^ " scheme")

let assert_no_scheme actual =
  match actual with
  | None -> Ok ()
  | Some (actual: TypeScheme.t) ->
      Error ("expected no scheme but found " ^ Type.to_string actual.body)

let assert_type_decl_name ~expected (actual: Ast.type_declaration option) =
  match actual with
  | Some actual when SurfacePath.equal actual.Ast.name (ident expected) -> Ok ()
  | Some actual ->
      Error ("expected type " ^ expected ^ " but found " ^ SurfacePath.to_string actual.name)
  | None -> Error ("expected type " ^ expected)

let assert_no_type_decl (actual: Ast.type_declaration option) =
  match actual with
  | None -> Ok ()
  | Some actual ->
      Error ("expected no type declaration but found " ^ SurfacePath.to_string actual.Ast.name)

let assert_true ~msg value =
  if value then
    Ok ()
  else
    Error msg

let assert_false ~msg value = assert_true ~msg (not value)

let export_names env =
  Env.exports env
  |> Iter.Iterator.map ~fn:(fun (name, _) -> SurfacePath.to_string name)
  |> Iter.Iterator.to_list

let exported_type_names env =
  Env.exported_types env
  |> Iter.Iterator.map ~fn:(fun (name, _) -> SurfacePath.to_string name)
  |> Iter.Iterator.to_list

let assert_names ~expected actual =
  if actual = expected then
    Ok ()
  else
    Error ("expected ["
    ^ String.concat ", " expected
    ^ "] but found ["
    ^ String.concat ", " actual
    ^ "]")

let test_value_scope_shadows_and_pops _ctx =
  let name = ident "value" in
  let env = Env.create () in
  let env = Env.add_value env ~name ~scheme:(scheme (int_type ())) in
  let env = Env.push_scope env in
  let env = Env.add_value env ~name ~scheme:(scheme (bool_type ())) in
  let* () = assert_scheme_body ~expected:(bool_type ()) (Env.get_value env ~name) in
  let env = Env.pop_scope env in
  assert_scheme_body ~expected:(int_type ()) (Env.get_value env ~name)

let test_exports_ignore_local_value_scopes _ctx =
  let root_name = ident "root_value" in
  let local_name = ident "local_value" in
  let env = Env.create () in
  let env = Env.add_value env ~name:root_name ~scheme:(scheme (int_type ())) in
  let env = Env.push_scope env in
  let env = Env.add_value env ~name:local_name ~scheme:(scheme (bool_type ())) in
  let* () = assert_scheme_body ~expected:(bool_type ()) (Env.get_value env ~name:local_name) in
  assert_names ~expected:[ "root_value" ] (export_names env)

let test_types_are_current_module_not_lexical_scope _ctx =
  let name = ident "color" in
  let env = Env.create () in
  let env = Env.push_scope env in
  let env = Env.add_type env ~name ~declaration:(type_decl "color") in
  let* () = assert_type_decl_name ~expected:"color" (Env.get_type env ~name) in
  let env = Env.pop_scope env in
  let* () = assert_type_decl_name ~expected:"color" (Env.get_type env ~name) in
  assert_names ~expected:[ "color" ] (exported_type_names env)

let test_constructors_are_current_module_not_lexical_scope _ctx =
  let name = ident "Red" in
  let env = Env.create () in
  let env = Env.push_scope env in
  let env = Env.add_constructor env ~name ~scheme:(scheme (int_type ())) in
  let* () = assert_scheme_body ~expected:(int_type ()) (Env.get_constructor env ~name) in
  let env = Env.pop_scope env in
  assert_scheme_body ~expected:(int_type ()) (Env.get_constructor env ~name)

let test_module_sees_parent_types_but_exports_only_own_types _ctx =
  let parent_type = ident "a" in
  let child_type = ident "b" in
  let module_name = ident "M" in
  let env = Env.create () in
  let env = Env.add_type env ~name:parent_type ~declaration:(type_decl "a") in
  let env = Env.push_module env ~name:module_name in
  let* () = assert_type_decl_name ~expected:"a" (Env.get_type env ~name:parent_type) in
  let env = Env.add_type env ~name:child_type ~declaration:(type_decl "b") in
  let* () = assert_type_decl_name ~expected:"b" (Env.get_type env ~name:child_type) in
  let env = Env.pop_module env in
  let* () = assert_no_type_decl (Env.get_type env ~name:child_type) in
  match Env.get_module env ~name:module_name with
  | None -> Error "expected module M"
  | Some summary ->
      let* () =
        assert_false
          ~msg:"module summary should not copy parent type a"
          (Env.module_has_type summary ~name:parent_type)
      in
      assert_true
        ~msg:"module summary should export own type b"
        (Env.module_has_type summary ~name:child_type)

let test_nested_modules_resolve_upward_and_export_downward _ctx =
  let root_type = ident "a" in
  let middle_type = ident "b" in
  let inner_type = ident "c" in
  let m1_name = ident "M1" in
  let m2_name = ident "M2" in
  let env = Env.create () in
  let env = Env.add_type env ~name:root_type ~declaration:(type_decl "a") in
  let env = Env.push_module env ~name:m1_name in
  let env = Env.add_type env ~name:middle_type ~declaration:(type_decl "b") in
  let env = Env.push_module env ~name:m2_name in
  let* () = assert_type_decl_name ~expected:"a" (Env.get_type env ~name:root_type) in
  let* () = assert_type_decl_name ~expected:"b" (Env.get_type env ~name:middle_type) in
  let env = Env.add_type env ~name:inner_type ~declaration:(type_decl "c") in
  let env = Env.pop_module env in
  let* () = assert_no_type_decl (Env.get_type env ~name:inner_type) in
  let* () =
    match Env.get_module env ~name:m2_name with
    | None -> Error "expected nested module M2 while inside M1"
    | Some m2 ->
        assert_true ~msg:"M2 should export own type c" (Env.module_has_type m2 ~name:inner_type)
  in
  let env = Env.pop_module env in
  match Env.get_module env ~name:m1_name with
  | None -> Error "expected module M1"
  | Some m1 ->
      let* () =
        assert_true ~msg:"M1 should export own type b" (Env.module_has_type m1 ~name:middle_type)
      in
      (
        match Env.module_get_module m1 ~name:m2_name with
        | None -> Error "expected M1.M2"
        | Some m2 ->
            assert_true
              ~msg:"M1.M2 should export own type c"
              (Env.module_has_type m2 ~name:inner_type)
      )

let test_module_values_see_parent_values_but_do_not_leak _ctx =
  let root_value = ident "root_value" in
  let child_value = ident "child_value" in
  let module_name = ident "M" in
  let env = Env.create () in
  let env = Env.add_value env ~name:root_value ~scheme:(scheme (int_type ())) in
  let env = Env.push_module env ~name:module_name in
  let* () = assert_scheme_body ~expected:(int_type ()) (Env.get_value env ~name:root_value) in
  let env = Env.add_value env ~name:child_value ~scheme:(scheme (bool_type ())) in
  let env = Env.pop_module env in
  let* () = assert_no_scheme (Env.get_value env ~name:child_value) in
  match Env.get_module env ~name:module_name with
  | None -> Error "expected module M"
  | Some summary ->
      assert_true
        ~msg:"module summary should export own value"
        (Env.module_has_value summary ~name:child_value)

let tests =
  Test.[
    case "infer-env: value scope shadows and pops" test_value_scope_shadows_and_pops;
    case "infer-env: exports ignore local value scopes" test_exports_ignore_local_value_scopes;
    case
      "infer-env: types are current-module not lexical-scope"
      test_types_are_current_module_not_lexical_scope;
    case
      "infer-env: constructors are current-module not lexical-scope"
      test_constructors_are_current_module_not_lexical_scope;
    case
      "infer-env: module sees parent types but exports only own types"
      test_module_sees_parent_types_but_exports_only_own_types;
    case
      "infer-env: nested modules resolve upward and export downward"
      test_nested_modules_resolve_upward_and_export_downward;
    case
      "infer-env: module values see parent values but do not leak"
      test_module_values_see_parent_values_but_do_not_leak;
  ]

let main ~args = Test.Cli.main ~name:"typ:infer-env" ~tests ~args ()

let () = Runtime.run ~main ~args:Std.Env.args ()
