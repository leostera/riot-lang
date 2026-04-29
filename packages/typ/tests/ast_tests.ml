open Std

module Ast = Typ.Ast
module Type = Typ.Ast.Type
module TypeVar = Typ.Ast.TypeVar
module SurfacePath = Typ.Model.Surface_path

let mk_path name = SurfacePath.from_name name

let int_type = Type.Constructor { ident = mk_path "int"; arguments = [] }

let bool_type = Type.Constructor { ident = mk_path "bool"; arguments = [] }

let list_type argument = Type.Constructor { ident = mk_path "list"; arguments = [ argument ] }

let arrow ?(label = Type.Label.NoLabel) parameter result = Type.Arrow { label; parameter; result }

let assert_equal_type left right =
  if Type.equal left right then
    Ok ()
  else
    Error ("expected " ^ Type.to_string left ^ " to equal " ^ Type.to_string right)

let assert_not_equal_type left right =
  if Type.equal left right then
    Error ("expected " ^ Type.to_string left ^ " not to equal " ^ Type.to_string right)
  else
    Ok ()

let variable ?link id = Type.Var { id; link }

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create typ ast test source slice"

let parse_typ_ast = fun source ->
  let parse_result = Syn.parse ~filename:(Path.v "test.ml") (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  Ast.from_parse_result ~source:model_source parse_result
  |> Result.expect ~msg:"expected typ ast build"

let assert_path_string ~expected actual =
  Test.assert_equal ~expected ~actual:(SurfacePath.to_string actual);
  Ok ()

let test_equal_follows_linked_variables _ctx =
  let linked = variable ~link:int_type TypeVar.first in
  assert_equal_type linked int_type

let test_equal_compares_unlinked_variable_ids _ctx =
  let first = variable TypeVar.first in
  let same_first = variable TypeVar.first in
  let second = variable (TypeVar.next TypeVar.first) in
  match assert_equal_type first same_first with
  | Error _ as error -> error
  | Ok () -> assert_not_equal_type first second

let test_equal_tuple_arity_mismatch_is_false _ctx =
  let one = Type.Tuple [ int_type ] in
  let two = Type.Tuple [ int_type; bool_type ] in
  assert_not_equal_type one two

let test_equal_constructor_arguments_are_structural _ctx =
  let int_list = list_type int_type in
  let same_int_list = list_type int_type in
  let bool_list = list_type bool_type in
  match assert_equal_type int_list same_int_list with
  | Error _ as error -> error
  | Ok () -> assert_not_equal_type int_list bool_list

let test_equal_arrows_compare_labels_and_children _ctx =
  let unlabeled = arrow int_type bool_type in
  let same_unlabeled = arrow int_type bool_type in
  let labeled = arrow ~label:(Type.Label.Labelled "value") int_type bool_type in
  match assert_equal_type unlabeled same_unlabeled with
  | Error _ as error -> error
  | Ok () -> assert_not_equal_type unlabeled labeled

let test_from_syn_keeps_constructor_patterns _ctx =
  let ast =
    parse_typ_ast
      {ocaml|let value = function
  | Some x -> x
  | None -> 0
|ocaml}
  in
  match ast.kind with
  | Implementation [
      {
        kind = Let {
          bindings = [
            {
              body = { kind = Function { body = Cases [ some_case; none_case ]; _ }; _ };
              _;
            };
          ];
          _;
        };
        _;
      };
    ] ->
      let some_result =
        match some_case.pattern.kind with
        | Apply {
          callee = { kind = Constructor { ident = constructor }; _ };
          argument = { kind = Bind binding; _ };
        } ->
            (
              match assert_path_string ~expected:"Some" constructor with
              | Error _ as error -> error
              | Ok () -> assert_path_string ~expected:"x" binding
            )
        | _ -> Error "expected Some x to lower as constructor pattern application"
      in
      (
        match some_result with
        | Error _ as error -> error
        | Ok () -> (
            match none_case.pattern.kind with
            | Constructor { ident } -> assert_path_string ~expected:"None" ident
            | _ -> Error "expected None to lower as constructor pattern"
          )
      )
  | _ -> Error "expected function with Some and None cases"

let tests =
  Test.[
    case "type equal follows linked variables" test_equal_follows_linked_variables;
    case "type equal compares unlinked variable ids" test_equal_compares_unlinked_variable_ids;
    case "type equal tuple arity mismatch is false" test_equal_tuple_arity_mismatch_is_false;
    case
      "type equal constructor arguments are structural"
      test_equal_constructor_arguments_are_structural;
    case
      "type equal arrows compare labels and children"
      test_equal_arrows_compare_labels_and_children;
    case
      "from syn keeps constructor patterns"
      test_from_syn_keeps_constructor_patterns;
  ]

let main ~args = Test.Cli.main ~name:"typ:ast" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
