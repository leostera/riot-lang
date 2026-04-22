open Std
open Std.Collections
open Syn
module Ast2 = Syn.Ast2
module Iterator = Iter.Iterator

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create source slice: " ^ Kernel.IO.Error.message error)

let diagnostics_to_string = fun diagnostics ->
  let items = ref [] in
  Vector.iter diagnostics
  |> Iterator.for_each ~fn:(fun diagnostic -> items := Diagnostic.to_string diagnostic :: !items);
  List.reverse !items |> String.concat "\n"

let expect_some = fun value ~msg ->
  match value with
  | Some value -> Ok value
  | None -> Error msg

let require_some = fun value ~msg -> expect_some value ~msg |> Result.expect ~msg

let parse_root = fun ~filename source ->
  let source = source_slice source in
  let parse_result = Syn.parse2 ~filename source in
  if Vector.length parse_result.Parser2.diagnostics > 0 then
    Error ("unexpected parse2 diagnostics:\n" ^ diagnostics_to_string parse_result.Parser2.diagnostics)
  else
    Ok (Ast2.SourceFile.make parse_result.Parser2.tree)

let parse_ml = parse_root ~filename:(Path.v "sample.ml")

let parse_mli = parse_root ~filename:(Path.v "sample.mli")

let nth_structure_item = fun (root: Ast2.source_file) target ->
  let found = ref None in
  let seen = ref 0 in
  Ast2.SourceFile.for_each_structure_item root
    ~fn:(fun item ->
      match !found with
      | Some _ -> ()
      | None ->
          if Int.equal !seen target then
            found := Some item
          else
            seen := !seen + 1);
  !found

let nth_signature_item = fun (root: Ast2.source_file) target ->
  let found = ref None in
  let seen = ref 0 in
  Ast2.SourceFile.for_each_signature_item root
    ~fn:(fun item ->
      match !found with
      | Some _ -> ()
      | None ->
          if Int.equal !seen target then
            found := Some item
          else
            seen := !seen + 1);
  !found

let binding_of_structure_item = fun item ->
  match Ast2.StructureItem.view item with
  | Ast2.StructureItem.Let decl -> Ast2.LetDeclaration.first_binding decl |> expect_some ~msg:"expected first let binding"
  | _ -> Error "expected let structure item"

let body_of_binding = fun binding -> Ast2.LetBinding.body binding |> expect_some ~msg:"expected let binding body"

let pattern_of_binding = fun binding -> Ast2.LetBinding.pattern binding |> expect_some ~msg:"expected let binding pattern"

let assert_last_ident_text = fun path expected ->
  let token = Ast2.Path.last_ident path |> require_some ~msg:"expected last path ident" in
  Test.assert_equal ~expected ~actual:(Ast2.Token.text token)

let assert_type_path_last_ident = fun type_expr expected ->
  match Ast2.TypeExpr.view type_expr with
  | Ast2.TypeExpr.Path { path } -> assert_last_ident_text path expected
  | _ -> panic "expected path type"

let test_source_file_and_let_binding_views = fun _ctx ->
  let root = parse_ml "let x = 1\n" |> Result.expect ~msg:"expected parse2 source file" in
  (
    match Ast2.SourceFile.view root with
    | Ast2.SourceFile.Implementation _ -> ()
    | _ -> panic "expected implementation root"
  );
  let item = nth_structure_item root 0 |> require_some ~msg:"expected first structure item" in
  let binding = binding_of_structure_item item |> Result.expect ~msg:"expected let binding" in
  let pattern = pattern_of_binding binding |> Result.expect ~msg:"expected binding pattern" in
  (
    match Ast2.Pattern.view pattern with
    | Ast2.Pattern.Path { path } -> assert_last_ident_text path "x"
    | _ -> panic "expected path pattern"
  );
  let body = body_of_binding binding |> Result.expect ~msg:"expected binding body" in
  (
    match Ast2.Expr.view body with
    | Ast2.Expr.Literal -> Ok ()
    | _ -> Error "expected literal expression body"
  )

let test_expression_views = fun _ctx ->
  let source = "let x = if ready then 1 else 2\nlet y = match x with | 0 -> 1 | _ -> 2\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let if_item = nth_structure_item root 0 |> require_some ~msg:"expected first structure item" in
  let if_body = if_item
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected first let binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected if body" in
  (
    match Ast2.Expr.view if_body with
    | Ast2.Expr.If { condition; then_branch; else_branch } ->
        ignore
          (condition |> expect_some ~msg:"expected if condition" |> Result.expect ~msg:"condition");
        ignore
          (then_branch |> expect_some ~msg:"expected then branch" |> Result.expect ~msg:"then branch");
        ignore
          (else_branch |> expect_some ~msg:"expected else branch" |> Result.expect ~msg:"else branch")
    | _ -> panic "expected if expression"
  );
  let match_item = nth_structure_item root 1 |> require_some ~msg:"expected second structure item" in
  let match_body = match_item
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected second let binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected match body" in
  match Ast2.Expr.view match_body with
  | Ast2.Expr.Match { scrutinee; first_case } ->
      ignore
        (scrutinee |> expect_some ~msg:"expected match scrutinee" |> Result.expect ~msg:"scrutinee");
      let first_case = first_case |> require_some ~msg:"expected first match case" in
      let case = Ast2.MatchCase.view first_case in
      (
        match case.Ast2.MatchCase.guard with
        | None -> ()
        | Some _ -> panic "expected first match case without guard"
      );
      ignore
        (case.Ast2.MatchCase.pattern
        |> expect_some ~msg:"expected case pattern"
        |> Result.expect ~msg:"case pattern");
      ignore
        (case.Ast2.MatchCase.body |> expect_some ~msg:"expected case body" |> Result.expect ~msg:"case body");
      Ok ()
  | _ -> Error "expected match expression"

let test_pattern_views = fun _ctx ->
  let source = "let (a, b) = xs\nlet h :: t = xs\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let tuple_pattern = nth_structure_item root 0
  |> require_some ~msg:"expected tuple pattern item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected tuple pattern binding"
  |> pattern_of_binding
  |> Result.expect ~msg:"expected tuple pattern" in
  (
    match Ast2.Pattern.view tuple_pattern with
    | Ast2.Pattern.Parenthesized { inner=Some inner } -> (
        match Ast2.Pattern.view inner with
        | Ast2.Pattern.Tuple -> ()
        | _ -> panic "expected tuple pattern inside parentheses"
      )
    | _ -> panic "expected tuple pattern"
  );
  let cons_pattern = nth_structure_item root 1
  |> require_some ~msg:"expected cons pattern item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected cons pattern binding"
  |> pattern_of_binding
  |> Result.expect ~msg:"expected cons pattern" in
  match Ast2.Pattern.view cons_pattern with
  | Ast2.Pattern.Cons { head; tail } ->
      ignore
        (head |> expect_some ~msg:"expected cons head" |> Result.expect ~msg:"cons head");
      ignore
        (tail |> expect_some ~msg:"expected cons tail" |> Result.expect ~msg:"cons tail");
      Ok ()
  | _ -> Error "expected cons pattern"

let test_signature_and_type_views = fun _ctx ->
  let root = parse_mli "val x : int -> string\ntype t = int\nmodule M : sig end\n"
  |> Result.expect ~msg:"expected parse2 interface" in
  (
    match Ast2.SourceFile.view root with
    | Ast2.SourceFile.Interface _ -> ()
    | _ -> panic "expected interface root"
  );
  let value_item = nth_signature_item root 0 |> require_some ~msg:"expected value signature item" in
  (
    match Ast2.SignatureItem.view value_item with
    | Ast2.SignatureItem.Value decl ->
        let name = Ast2.ValueDeclaration.name decl |> require_some ~msg:"expected value name" in
        Test.assert_equal ~expected:"x" ~actual:(Ast2.Token.text name);
        let annotation = Ast2.ValueDeclaration.type_annotation decl |> require_some ~msg:"expected value type annotation" in
        (
          match Ast2.TypeExpr.view annotation with
          | Ast2.TypeExpr.Arrow { left=Some left; right=Some right } ->
              assert_type_path_last_ident left "int";
              assert_type_path_last_ident right "string";
              Ok ()
          | _ -> Error "expected arrow value type"
        )
    | _ -> Error "expected value declaration"
  ) |> Result.expect ~msg:"expected value signature";
  let type_item = nth_signature_item root 1 |> require_some ~msg:"expected type signature item" in
  (
    match Ast2.SignatureItem.view type_item with
    | Ast2.SignatureItem.Type decl ->
        let name = Ast2.TypeDeclaration.name decl |> require_some ~msg:"expected type name" in
        Test.assert_equal ~expected:"t" ~actual:(Ast2.Token.text name);
        let manifest = Ast2.TypeDeclaration.manifest decl |> require_some ~msg:"expected type manifest" in
        assert_type_path_last_ident manifest "int"
    | _ -> panic "expected type declaration"
  );
  let module_item = nth_signature_item root 2 |> require_some ~msg:"expected module signature item" in
  match Ast2.SignatureItem.view module_item with
  | Ast2.SignatureItem.Module decl ->
      let name = Ast2.ModuleDeclaration.name decl |> require_some ~msg:"expected module name" in
      Test.assert_equal ~expected:"M" ~actual:(Ast2.Token.text name);
      Ok ()
  | _ -> Error "expected module declaration"

let test_type_expression_views = fun _ctx ->
  let root = parse_mli "val xs : int list\nexternal id : 'a -> 'a = \"%identity\"\n"
  |> Result.expect ~msg:"expected parse2 interface" in
  let value_item = nth_signature_item root 0 |> require_some ~msg:"expected value signature item" in
  (
    match Ast2.SignatureItem.view value_item with
    | Ast2.SignatureItem.Value decl ->
        let annotation = Ast2.ValueDeclaration.type_annotation decl |> require_some ~msg:"expected value type annotation" in
        (
          match Ast2.TypeExpr.view annotation with
          | Ast2.TypeExpr.Apply { argument=Some argument; constructor=Some constructor } ->
              assert_type_path_last_ident argument "int";
              assert_type_path_last_ident constructor "list";
              Ok ()
          | _ -> Error "expected type application"
        )
    | _ -> Error "expected value declaration"
  ) |> Result.expect ~msg:"expected value type application";
  let external_item = nth_signature_item root 1 |> require_some ~msg:"expected external signature item" in
  match Ast2.SignatureItem.view external_item with
  | Ast2.SignatureItem.External decl ->
      let annotation = Ast2.ExternalDeclaration.type_annotation decl |> require_some ~msg:"expected external type annotation" in
      (
        match Ast2.TypeExpr.view annotation with
        | Ast2.TypeExpr.Arrow { left=Some left; right=Some right } -> (
            match Ast2.TypeExpr.view left, Ast2.TypeExpr.view right with
            | Ast2.TypeExpr.Var { name=Some left_name }, Ast2.TypeExpr.Var { name=Some right_name } ->
                Test.assert_equal ~expected:"a" ~actual:(Ast2.Token.text left_name);
                Test.assert_equal ~expected:"a" ~actual:(Ast2.Token.text right_name);
                Ok ()
            | _ -> Error "expected type variables"
          )
        | _ -> Error "expected external arrow type"
      )
  | _ -> Error "expected external declaration"

let assert_type_manifest_is_none = fun source ->
  let root = parse_mli source |> Result.expect ~msg:"expected parse2 interface" in
  let type_item = nth_signature_item root 0 |> require_some ~msg:"expected type signature item" in
  match Ast2.SignatureItem.view type_item with
  | Ast2.SignatureItem.Type decl -> (
      match Ast2.TypeDeclaration.manifest decl with
      | None -> Ok ()
      | Some _ -> Error "expected type declaration without manifest view"
    )
  | _ -> Error "expected type declaration"

let test_non_manifest_type_declaration_bodies = fun _ctx ->
  match assert_type_manifest_is_none "type color = Red | Blue\n" with
  | Error _ as error -> error
  | Ok () ->
      assert_type_manifest_is_none "type point = { x : int }\n"

let test_type_declaration_parameters = fun _ctx ->
  let root = parse_mli "type (+'a, _) box = 'a list\n" |> Result.expect ~msg:"expected parse2 interface" in
  let type_item = nth_signature_item root 0 |> require_some ~msg:"expected type signature item" in
  match Ast2.SignatureItem.view type_item with
  | Ast2.SignatureItem.Type decl ->
      let name = Ast2.TypeDeclaration.name decl |> require_some ~msg:"expected type name" in
      Test.assert_equal ~expected:"box" ~actual:(Ast2.Token.text name);
      let named = ref None in
      let wildcard_param = ref None in
      Ast2.TypeDeclaration.for_each_parameter decl
        ~fn:(
          function
          | Ast2.TypeDeclaration.Named { name; variance; _ } ->
              named := Some (Ast2.Token.text name, Option.map variance ~fn:Ast2.Token.text)
          | Ast2.TypeDeclaration.Wildcard { wildcard; _ } ->
              wildcard_param := Some (Ast2.Token.text wildcard)
        );
      (
        match !named with
        | Some ("a", Some "+") -> ()
        | _ -> panic "expected covariant named type parameter"
      );
      (
        match !wildcard_param with
        | Some "_" -> Ok ()
        | _ -> Error "expected wildcard type parameter"
      )
  | _ -> Error "expected type declaration"

let test_binding_type_annotation_view = fun _ctx ->
  let root = parse_ml "let x : int = 1\n" |> Result.expect ~msg:"expected parse2 source file" in
  let binding = nth_structure_item root 0
  |> require_some ~msg:"expected first structure item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected let binding" in
  let annotation = Ast2.LetBinding.type_annotation binding |> require_some ~msg:"expected binding type annotation" in
  match Ast2.TypeExpr.view annotation with
  | Ast2.TypeExpr.Path { path } ->
      assert_last_ident_text path "int";
      Ok ()
  | _ -> Error "expected binding path type annotation"

let tests = [
  Test.case "ast2 exposes source file and let binding views" test_source_file_and_let_binding_views;
  Test.case "ast2 exposes if and match expression views" test_expression_views;
  Test.case "ast2 exposes tuple and cons pattern views" test_pattern_views;
  Test.case "ast2 exposes signature declaration views" test_signature_and_type_views;
  Test.case "ast2 exposes type expression views" test_type_expression_views;
  Test.case
    "ast2 keeps non-manifest type bodies out of manifest views"
    test_non_manifest_type_declaration_bodies;
  Test.case "ast2 exposes type declaration parameters" test_type_declaration_parameters;
  Test.case "ast2 exposes let binding type annotation views" test_binding_type_annotation_view;
]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"syn-ast2" ~tests ~args ()) ~args:Env.args ()
