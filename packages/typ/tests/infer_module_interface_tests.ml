open Std
open Std.Collections
open Std.Iter
open Std.Result.Syntax

module Ast = Typ.Ast
module Env = Typ.Infer.Env
module ModuleInterface = Typ.Infer.ModuleInterface
module SurfacePath = Typ.Model.Surface_path
module Type = Typ.Ast.Type
module TypeScheme = Typ.Infer.TypeScheme

let ident name =
  Syn.parse_ident name
  |> Option.map ~fn:SurfacePath.from_syn_ident
  |> Option.expect ~msg:("expected surface path test identifier " ^ name)

let nominal name = Type.Apply { ident = ident name; arguments = [] }

let int_type = fun () -> nominal "int"

let bool_type = fun () -> nominal "bool"

let scheme type_ = TypeScheme.monomorphic type_

let origin: Ast.origin = {
  span = Syn.Span.make ~start:0 ~end_:0;
  kind = Syn.SyntaxKind.TYPE_DECL;
}

let type_definition kind: Ast.type_definition = { origin; kind }

let constructor name: Ast.type_constructor = {
  origin;
  name = ident name;
  arguments = Tuple [];
  result = None;
}

let abstract_type name: Ast.type_declaration = {
  origin;
  name = ident name;
  parameters = [];
  definition = type_definition Abstract;
}

let variant_type name constructors: Ast.type_declaration = {
  origin;
  name = ident name;
  parameters = [];
  definition = type_definition (Variant (List.map constructors ~fn:constructor));
}

let render_interface intf =
  Typ.SignatureGenerator.from_exports
    ~types:(ModuleInterface.types intf)
    ~values:(
      intf
      |> ModuleInterface.values
      |> Iterator.map ~fn:(fun (name, (type_scheme: TypeScheme.t)) -> (name, type_scheme.body))
    )

let names items =
  items
  |> Iterator.map ~fn:(fun (name, _) -> SurfacePath.to_string name)
  |> Iterator.to_list

let assert_equal_string ~expected actual =
  if String.equal expected actual then
    Ok ()
  else
    Error ("expected:\n" ^ expected ^ "\nbut found:\n" ^ actual)

let assert_equal_names ~expected actual =
  if actual = expected then
    Ok ()
  else
    Error ("expected ["
    ^ String.concat ", " expected
    ^ "] but found ["
    ^ String.concat ", " actual
    ^ "]")

let find_module intf name =
  ModuleInterface.modules intf
  |> Iterator.find ~fn:(fun (module_name, _) -> SurfacePath.equal module_name (ident name))
  |> Option.map ~fn:(fun (_, intf) -> intf)

let test_empty_env_renders_empty_interface _ctx =
  Env.create ()
  |> ModuleInterface.from_env
  |> render_interface
  |> assert_equal_string ~expected:""

let test_env_with_one_type_renders_type _ctx =
  let env = Env.create () in
  let env =
    Env.add_type
      env
      ~name:(ident "color")
      ~declaration:(variant_type "color" [ "Red"; "Blue" ])
  in
  env
  |> ModuleInterface.from_env
  |> render_interface
  |> assert_equal_string ~expected:"type color = Red | Blue\n"

let test_env_with_one_value_renders_value _ctx =
  let env = Env.create () in
  let env = Env.add_value env ~name:(ident "answer") ~scheme:(scheme (int_type ())) in
  env
  |> ModuleInterface.from_env
  |> render_interface
  |> assert_equal_string ~expected:"val answer : int\n"

let test_local_values_do_not_become_exports _ctx =
  let env = Env.create () in
  let env = Env.add_value env ~name:(ident "root") ~scheme:(scheme (int_type ())) in
  let env = Env.push_scope env in
  let env = Env.add_value env ~name:(ident "local") ~scheme:(scheme (bool_type ())) in
  let intf = ModuleInterface.from_env env in
  ModuleInterface.values intf
  |> names
  |> assert_equal_names ~expected:[ "root" ]

let test_multiple_modules_are_copied_recursively _ctx =
  let env = Env.create () in
  let env = Env.push_module env ~name:(ident "M") in
  let env = Env.add_value env ~name:(ident "value") ~scheme:(scheme (int_type ())) in
  let env = Env.push_module env ~name:(ident "Inner") in
  let env = Env.add_type env ~name:(ident "t") ~declaration:(abstract_type "t") in
  let env = Env.pop_module env in
  let env = Env.pop_module env in
  let env = Env.push_module env ~name:(ident "N") in
  let env = Env.add_value env ~name:(ident "flag") ~scheme:(scheme (bool_type ())) in
  let env = Env.pop_module env in
  let intf = ModuleInterface.from_env env in
  let* () =
    ModuleInterface.modules intf
    |> names
    |> assert_equal_names ~expected:[ "M"; "N" ]
  in
  match (find_module intf "M", find_module intf "N") with
  | (Some m, Some n) ->
      let* () =
        ModuleInterface.values m
        |> names
        |> assert_equal_names ~expected:[ "value" ]
      in
      let* () =
        ModuleInterface.modules m
        |> names
        |> assert_equal_names ~expected:[ "Inner" ]
      in
      ModuleInterface.values n
      |> names
      |> assert_equal_names ~expected:[ "flag" ]
  | _ -> Error "expected modules M and N"

let tests =
  Test.[
    case "empty env renders empty interface" test_empty_env_renders_empty_interface;
    case "one type renders type" test_env_with_one_type_renders_type;
    case "one value renders value" test_env_with_one_value_renders_value;
    case "local values do not become exports" test_local_values_do_not_become_exports;
    case "multiple modules are copied recursively" test_multiple_modules_are_copied_recursively;
  ]

let main ~args = Test.Cli.main ~name:"typ:infer-module-interface" ~tests ~args ()

let () = Runtime.run ~main ~args:Std.Env.args ()
