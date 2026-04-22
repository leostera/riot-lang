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
    | Ast2.Expr.Literal { token=Some token } ->
        Test.assert_equal ~expected:"1" ~actual:(Ast2.Token.text token);
        Ok ()
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
  | Ok () -> assert_type_manifest_is_none "type point = { x : int }\n"

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
          | Ast2.TypeDeclaration.Named { name; variance; _ } -> named := Some (
            Ast2.Token.text name,
            Option.map variance ~fn:Ast2.Token.text
          )
          | Ast2.TypeDeclaration.Wildcard { wildcard; _ } -> wildcard_param := Some (Ast2.Token.text
            wildcard)
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

let test_open_declaration_path_tokens = fun _ctx ->
  let root = parse_ml "open Foo.Bar\n" |> Result.expect ~msg:"expected parse2 source file" in
  let item = nth_structure_item root 0 |> require_some ~msg:"expected open structure item" in
  match Ast2.StructureItem.view item with
  | Ast2.StructureItem.Open decl ->
      let first = Ast2.OpenDeclaration.first_path_ident decl |> require_some ~msg:"expected first open path ident" in
      let last = Ast2.OpenDeclaration.last_path_ident decl |> require_some ~msg:"expected last open path ident" in
      let count = ref 0 in
      Ast2.OpenDeclaration.for_each_path_ident decl ~fn:(fun _ -> count := !count + 1);
      Test.assert_equal ~expected:"Foo" ~actual:(Ast2.Token.text first);
      Test.assert_equal ~expected:"Bar" ~actual:(Ast2.Token.text last);
      Test.assert_equal ~expected:2 ~actual:!count;
      Ok ()
  | _ -> Error "expected open declaration"

let test_simple_declaration_token_views = fun _ctx ->
  let source = "include Foo.Bar\nexternal id : 'a -> 'a = \"%identity\" \"caml_id\"\nexception Boom\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let include_item = nth_structure_item root 0 |> require_some ~msg:"expected include structure item" in
  (
    match Ast2.StructureItem.view include_item with
    | Ast2.StructureItem.Include decl ->
        let first = Ast2.IncludeDeclaration.first_path_ident decl |> require_some ~msg:"expected first include path ident" in
        let last = Ast2.IncludeDeclaration.last_path_ident decl |> require_some ~msg:"expected last include path ident" in
        let count = ref 0 in
        Ast2.IncludeDeclaration.for_each_path_ident decl ~fn:(fun _ -> count := !count + 1);
        Test.assert_equal ~expected:"Foo" ~actual:(Ast2.Token.text first);
        Test.assert_equal ~expected:"Bar" ~actual:(Ast2.Token.text last);
        Test.assert_equal ~expected:2 ~actual:!count
    | _ -> panic "expected include declaration"
  );
  let external_item = nth_structure_item root 1 |> require_some ~msg:"expected external structure item" in
  (
    match Ast2.StructureItem.view external_item with
    | Ast2.StructureItem.External decl ->
        let name = Ast2.ExternalDeclaration.name decl |> require_some ~msg:"expected external name" in
        Test.assert_equal ~expected:"id" ~actual:(Ast2.Token.text name);
        let primitives = ref [] in
        Ast2.ExternalDeclaration.for_each_primitive_string
          decl
          ~fn:(fun token -> primitives := Ast2.Token.text token :: !primitives);
        Test.assert_equal
          ~expected:[ "\"%identity\""; "\"caml_id\"" ]
          ~actual:(List.reverse !primitives)
    | _ -> panic "expected external declaration"
  );
  let exception_item = nth_structure_item root 2 |> require_some ~msg:"expected exception structure item" in
  match Ast2.StructureItem.view exception_item with
  | Ast2.StructureItem.Exception decl ->
      let name = Ast2.ExceptionDeclaration.name decl |> require_some ~msg:"expected exception name" in
      Test.assert_equal ~expected:"Boom" ~actual:(Ast2.Token.text name);
      Ok ()
  | _ -> Error "expected exception declaration"

let test_module_declaration_tokens = fun _ctx ->
  let root = parse_ml "module rec M = struct end\nmodule _ = struct end\nmodule Alias = Foo.Bar\n"
  |> Result.expect ~msg:"expected parse2 source file" in
  let first_item = nth_structure_item root 0 |> require_some ~msg:"expected first module item" in
  let second_item = nth_structure_item root 1 |> require_some ~msg:"expected second module item" in
  let third_item = nth_structure_item root 2 |> require_some ~msg:"expected third module item" in
  (
    match Ast2.StructureItem.view first_item with
    | Ast2.StructureItem.Module decl ->
        let rec_token = Ast2.ModuleDeclaration.rec_token decl |> require_some ~msg:"expected rec token" in
        let name = Ast2.ModuleDeclaration.name decl |> require_some ~msg:"expected module name" in
        Test.assert_equal ~expected:"rec" ~actual:(Ast2.Token.text rec_token);
        Test.assert_equal ~expected:"M" ~actual:(Ast2.Token.text name);
        Test.assert_equal
          ~expected:Ast2.ModuleDeclaration.EmptyStruct
          ~actual:(Ast2.ModuleDeclaration.body decl)
    | _ -> panic "expected first module declaration"
  );
  (
    match Ast2.StructureItem.view second_item with
    | Ast2.StructureItem.Module decl ->
        let name = Ast2.ModuleDeclaration.name decl |> require_some ~msg:"expected module wildcard name" in
        Test.assert_equal ~expected:"_" ~actual:(Ast2.Token.text name)
    | _ -> panic "expected second module declaration"
  );
  (
    match Ast2.StructureItem.view third_item with
    | Ast2.StructureItem.Module decl ->
        let separator = Ast2.ModuleDeclaration.separator_token decl |> require_some ~msg:"expected module separator" in
        let segments = ref [] in
        Ast2.ModuleDeclaration.for_each_body_path_ident
          decl
          ~fn:(fun token -> segments := Ast2.Token.text token :: !segments);
        Test.assert_equal ~expected:"=" ~actual:(Ast2.Token.text separator);
        Test.assert_equal
          ~expected:Ast2.ModuleDeclaration.Path
          ~actual:(Ast2.ModuleDeclaration.body decl);
        Test.assert_equal ~expected:[ "Foo"; "Bar" ] ~actual:(List.reverse !segments)
    | _ -> panic "expected third module declaration"
  );
  Ok ()

let test_module_type_declaration_tokens = fun _ctx ->
  let root = parse_mli "module type S = Foo.S\nmodule type Empty = sig end\nmodule type Abstract\n"
  |> Result.expect ~msg:"expected parse2 interface" in
  let first_item = nth_signature_item root 0 |> require_some ~msg:"expected first module type item" in
  let second_item = nth_signature_item root 1 |> require_some ~msg:"expected second module type item" in
  let third_item = nth_signature_item root 2 |> require_some ~msg:"expected third module type item" in
  (
    match Ast2.SignatureItem.view first_item with
    | Ast2.SignatureItem.ModuleType decl ->
        let name = Ast2.ModuleTypeDeclaration.name decl |> require_some ~msg:"expected module type name" in
        let equals = Ast2.ModuleTypeDeclaration.equals_token decl |> require_some ~msg:"expected module type equals token" in
        let segments = ref [] in
        Ast2.ModuleTypeDeclaration.for_each_body_path_ident
          decl
          ~fn:(fun token -> segments := Ast2.Token.text token :: !segments);
        Test.assert_equal ~expected:"S" ~actual:(Ast2.Token.text name);
        Test.assert_equal ~expected:"=" ~actual:(Ast2.Token.text equals);
        Test.assert_equal
          ~expected:Ast2.ModuleTypeDeclaration.Path
          ~actual:(Ast2.ModuleTypeDeclaration.body decl);
        Test.assert_equal ~expected:[ "Foo"; "S" ] ~actual:(List.reverse !segments)
    | _ -> panic "expected first module type declaration"
  );
  (
    match Ast2.SignatureItem.view second_item with
    | Ast2.SignatureItem.ModuleType decl -> Test.assert_equal
      ~expected:Ast2.ModuleTypeDeclaration.EmptySig
      ~actual:(Ast2.ModuleTypeDeclaration.body decl)
    | _ -> panic "expected second module type declaration"
  );
  (
    match Ast2.SignatureItem.view third_item with
    | Ast2.SignatureItem.ModuleType decl -> Test.assert_equal
      ~expected:Ast2.ModuleTypeDeclaration.Abstract
      ~actual:(Ast2.ModuleTypeDeclaration.body decl)
    | _ -> panic "expected third module type declaration"
  );
  Ok ()

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

let last_path_text = fun path ->
  let token = Ast2.Path.last_ident path |> require_some ~msg:"expected path ident" in
  Ast2.Token.text token

let test_record_views = fun _ctx ->
  let source = "let record = { x = 1; y }\nlet updated = { base with x = 2; y }\nlet { x; y = z; _ } = record\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let record_expr = nth_structure_item root 0
  |> require_some ~msg:"expected record item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected record binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected record expression" in
  let record_view = Ast2.RecordExpr.cast record_expr |> require_some ~msg:"expected record expression view" in
  let record_fields = ref [] in
  Ast2.RecordExpr.for_each_field record_view
    ~fn:(fun field ->
      let name = field.Ast2.RecordExpr.path |> require_some ~msg:"expected record field path" |> last_path_text in
      record_fields := (name, Option.is_some field.Ast2.RecordExpr.value) :: !record_fields);
  Test.assert_equal ~expected:[ ("x", true); ("y", false) ] ~actual:(List.reverse !record_fields);
  let update_expr = nth_structure_item root 1
  |> require_some ~msg:"expected update item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected update binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected update expression" in
  let update_view = Ast2.RecordExpr.cast update_expr |> require_some ~msg:"expected record update view" in
  (
    match Ast2.RecordExpr.base update_view with
    | Some base -> (
        match Ast2.Expr.view base with
        | Ast2.Expr.Path { path } -> Test.assert_equal ~expected:"base" ~actual:(last_path_text path)
        | _ -> panic "expected record update base path"
      )
    | None -> panic "expected record update base"
  );
  let update_fields = ref [] in
  Ast2.RecordExpr.for_each_field update_view
    ~fn:(fun field ->
      let name = field.Ast2.RecordExpr.path |> require_some ~msg:"expected update field path" |> last_path_text in
      update_fields := (name, Option.is_some field.Ast2.RecordExpr.value) :: !update_fields);
  Test.assert_equal ~expected:[ ("x", true); ("y", false) ] ~actual:(List.reverse !update_fields);
  let record_pattern = nth_structure_item root 2
  |> require_some ~msg:"expected record pattern item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected record pattern binding"
  |> pattern_of_binding
  |> Result.expect ~msg:"expected record pattern" in
  let pattern_view = Ast2.RecordPattern.cast record_pattern |> require_some ~msg:"expected record pattern view" in
  let pattern_fields = ref [] in
  Ast2.RecordPattern.for_each_field pattern_view
    ~fn:(fun field ->
      let name = field.Ast2.RecordPattern.path
      |> require_some ~msg:"expected pattern field path"
      |> last_path_text in
      pattern_fields := (name, Option.is_some field.Ast2.RecordPattern.pattern) :: !pattern_fields);
  Test.assert_equal ~expected:[ ("x", false); ("y", true) ] ~actual:(List.reverse !pattern_fields);
  let wildcard = Ast2.RecordPattern.open_wildcard pattern_view |> require_some ~msg:"expected open record wildcard" in
  Test.assert_equal ~expected:"_" ~actual:(Ast2.Token.text wildcard);
  Ok ()

let binding_pattern_text = fun binding ->
  let pattern = pattern_of_binding binding |> Result.expect ~msg:"expected binding pattern" in
  match Ast2.Pattern.view pattern with
  | Ast2.Pattern.Path { path } -> last_path_text path
  | _ -> panic "expected path binding pattern"

let binding_body_path_text = fun binding ->
  let body = body_of_binding binding |> Result.expect ~msg:"expected binding body" in
  match Ast2.Expr.view body with
  | Ast2.Expr.Path { path } -> last_path_text path
  | _ -> panic "expected path binding body"

let test_binding_operator_views = fun _ctx ->
  let root = parse_ml "let both = let+ x = a and+ y = b in pair x y\n" |> Result.expect ~msg:"expected parse2 source file" in
  let expr = nth_structure_item root 0
  |> require_some ~msg:"expected binding operator item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected outer let binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected binding operator expression" in
  let binding_operator = Ast2.BindingOperatorExpr.cast expr |> require_some ~msg:"expected binding operator view" in
  let clauses = ref [] in
  Ast2.BindingOperatorExpr.for_each_clause binding_operator
    ~fn:(fun clause ->
      let keyword = clause.Ast2.BindingOperatorExpr.keyword
      |> require_some ~msg:"expected binding operator keyword"
      |> Ast2.Token.text in
      let operator = clause.Ast2.BindingOperatorExpr.operator
      |> require_some ~msg:"expected binding operator suffix"
      |> Ast2.Token.text in
      clauses := (
        keyword,
        operator,
        binding_pattern_text clause.Ast2.BindingOperatorExpr.binding,
        binding_body_path_text clause.Ast2.BindingOperatorExpr.binding
      )
      :: !clauses);
  Test.assert_equal
    ~expected:[ ("let", "+", "x", "a"); ("and", "+", "y", "b") ]
    ~actual:(List.reverse !clauses);
  let in_token = Ast2.BindingOperatorExpr.in_token binding_operator |> require_some ~msg:"expected binding operator in token" in
  Test.assert_equal ~expected:"in" ~actual:(Ast2.Token.text in_token);
  (
    match Ast2.BindingOperatorExpr.body binding_operator with
    | Some body -> (
        match Ast2.Expr.view body with
        | Ast2.Expr.Apply _ -> Ok ()
        | _ -> Error "expected binding operator body application"
      )
    | None -> Error "expected binding operator body"
  )

let local_open_pattern_path_text = fun pattern ->
  let segments = ref [] in
  Ast2.LocalOpenPattern.for_each_module_path_ident
    pattern
    ~fn:(fun token -> segments := Ast2.Token.text token :: !segments);
  List.reverse !segments |> String.concat "."

let first_class_module_path_text = fun expr ->
  let segments = ref [] in
  Ast2.FirstClassModuleExpr.for_each_module_path_ident
    expr
    ~fn:(fun token -> segments := Ast2.Token.text token :: !segments);
  List.reverse !segments |> String.concat "."

let first_class_module_ascription_text = fun expr ->
  let segments = ref [] in
  Ast2.FirstClassModuleExpr.for_each_ascription_path_ident
    expr
    ~fn:(fun token -> segments := Ast2.Token.text token :: !segments);
  List.reverse !segments |> String.concat "."

let let_module_body_path_text = fun expr ->
  let segments = ref [] in
  Ast2.LetModuleExpr.for_each_module_body_path_ident
    expr
    ~fn:(fun token -> segments := Ast2.Token.text token :: !segments);
  List.reverse !segments |> String.concat "."

let test_local_open_views = fun _ctx ->
  let source = "let value = let open Foo.Bar in result\nlet Foo.Bar.(x) = value\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let local_open_expr = nth_structure_item root 0
  |> require_some ~msg:"expected local open expression item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected local open binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected local open body" in
  let local_open = Ast2.LocalOpenExpr.cast local_open_expr |> require_some ~msg:"expected local open expression view" in
  (
    match Ast2.LocalOpenExpr.view local_open with
    | Ast2.LocalOpenExpr.LetOpen {
      let_token=Some let_token;
      open_token=Some open_token;
      module_path=Some module_path;
      in_token=Some in_token;
      body=Some body;
      _
    } ->
        Test.assert_equal ~expected:"let" ~actual:(Ast2.Token.text let_token);
        Test.assert_equal ~expected:"open" ~actual:(Ast2.Token.text open_token);
        Test.assert_equal ~expected:"Bar" ~actual:(last_path_text module_path);
        Test.assert_equal ~expected:"in" ~actual:(Ast2.Token.text in_token);
        (
          match Ast2.Expr.view body with
          | Ast2.Expr.Path { path } -> Test.assert_equal
            ~expected:"result"
            ~actual:(last_path_text path)
          | _ -> panic "expected local open body path"
        )
    | _ -> panic "expected complete let open expression"
  );
  let local_open_pattern = nth_structure_item root 1
  |> require_some ~msg:"expected local open pattern item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected local open pattern binding"
  |> pattern_of_binding
  |> Result.expect ~msg:"expected local open pattern" in
  let local_open_pattern = Ast2.LocalOpenPattern.cast local_open_pattern |> require_some ~msg:"expected local open pattern view" in
  Test.assert_equal ~expected:"Foo.Bar" ~actual:(local_open_pattern_path_text local_open_pattern);
  let dot_token = Ast2.LocalOpenPattern.dot_token local_open_pattern |> require_some ~msg:"expected local open dot" in
  let inner = Ast2.LocalOpenPattern.pattern local_open_pattern |> require_some ~msg:"expected inner local open pattern" in
  Test.assert_equal ~expected:"." ~actual:(Ast2.Token.text dot_token);
  (
    match Ast2.Pattern.view inner with
    | Ast2.Pattern.Path { path } ->
        Test.assert_equal ~expected:"x" ~actual:(last_path_text path);
        Ok ()
    | _ -> Error "expected local open inner path pattern"
  )

let test_first_class_module_views = fun _ctx ->
  let source = "let packed = (module Foo.Bar)\nlet typed = (module Foo : S.T)\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let packed = nth_structure_item root 0
  |> require_some ~msg:"expected packed module item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected packed module binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected packed module body" in
  let packed = Ast2.FirstClassModuleExpr.cast packed |> require_some ~msg:"expected first-class module view" in
  Test.assert_equal
    ~expected:Ast2.FirstClassModuleExpr.ModulePath
    ~actual:(Ast2.FirstClassModuleExpr.module_path packed);
  Test.assert_equal
    ~expected:Ast2.FirstClassModuleExpr.NoAscription
    ~actual:(Ast2.FirstClassModuleExpr.ascription packed);
  Test.assert_equal ~expected:"Foo.Bar" ~actual:(first_class_module_path_text packed);
  let typed = nth_structure_item root 1
  |> require_some ~msg:"expected typed module item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected typed module binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected typed module body" in
  let typed = Ast2.FirstClassModuleExpr.cast typed |> require_some ~msg:"expected typed first-class module view" in
  Test.assert_equal
    ~expected:Ast2.FirstClassModuleExpr.ModulePath
    ~actual:(Ast2.FirstClassModuleExpr.module_path typed);
  Test.assert_equal
    ~expected:Ast2.FirstClassModuleExpr.PathAscription
    ~actual:(Ast2.FirstClassModuleExpr.ascription typed);
  Test.assert_equal ~expected:"Foo" ~actual:(first_class_module_path_text typed);
  Test.assert_equal ~expected:"S.T" ~actual:(first_class_module_ascription_text typed);
  let colon = Ast2.FirstClassModuleExpr.colon_token typed |> require_some ~msg:"expected first-class module colon" in
  Test.assert_equal ~expected:":" ~actual:(Ast2.Token.text colon);
  Ok ()

let test_let_module_expression_views = fun _ctx ->
  let source = "let value = let module M = Foo.Bar in result\nlet empty = let module Empty = struct end in done_\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let value_expr = nth_structure_item root 0
  |> require_some ~msg:"expected let module item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected outer binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected let module body" in
  let module_expr = Ast2.LetModuleExpr.cast value_expr |> require_some ~msg:"expected let module expression view" in
  let module_token = Ast2.LetModuleExpr.module_token module_expr |> require_some ~msg:"expected module token" in
  let name = Ast2.LetModuleExpr.name module_expr |> require_some ~msg:"expected module name" in
  let equals = Ast2.LetModuleExpr.equals_token module_expr |> require_some ~msg:"expected let module equals" in
  let in_token = Ast2.LetModuleExpr.in_token module_expr |> require_some ~msg:"expected let module in" in
  Test.assert_equal ~expected:"module" ~actual:(Ast2.Token.text module_token);
  Test.assert_equal ~expected:"M" ~actual:(Ast2.Token.text name);
  Test.assert_equal ~expected:"=" ~actual:(Ast2.Token.text equals);
  Test.assert_equal ~expected:"in" ~actual:(Ast2.Token.text in_token);
  Test.assert_equal
    ~expected:Ast2.LetModuleExpr.Path
    ~actual:(Ast2.LetModuleExpr.module_body module_expr);
  Test.assert_equal ~expected:"Foo.Bar" ~actual:(let_module_body_path_text module_expr);
  (
    match Ast2.LetModuleExpr.body module_expr with
    | Some body -> (
        match Ast2.Expr.view body with
        | Ast2.Expr.Path { path } -> Test.assert_equal
          ~expected:"result"
          ~actual:(last_path_text path)
        | _ -> panic "expected let module body path"
      )
    | None -> panic "expected let module expression body"
  );
  let empty_expr = nth_structure_item root 1
  |> require_some ~msg:"expected empty let module item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected empty outer binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected empty let module body" in
  let empty_module = Ast2.LetModuleExpr.cast empty_expr |> require_some ~msg:"expected empty let module view" in
  Test.assert_equal
    ~expected:Ast2.LetModuleExpr.EmptyStruct
    ~actual:(Ast2.LetModuleExpr.module_body empty_module);
  Ok ()

let tests = [
  Test.case "ast2 exposes source file and let binding views" test_source_file_and_let_binding_views;
  Test.case "ast2 exposes if and match expression views" test_expression_views;
  Test.case "ast2 exposes tuple and cons pattern views" test_pattern_views;
  Test.case "ast2 exposes signature declaration views" test_signature_and_type_views;
  Test.case "ast2 exposes type expression views" test_type_expression_views;
  Test.case "ast2 keeps non-manifest type bodies out of manifest views" test_non_manifest_type_declaration_bodies;
  Test.case "ast2 exposes type declaration parameters" test_type_declaration_parameters;
  Test.case "ast2 exposes open declaration path tokens" test_open_declaration_path_tokens;
  Test.case "ast2 exposes simple declaration token views" test_simple_declaration_token_views;
  Test.case "ast2 exposes module declaration tokens" test_module_declaration_tokens;
  Test.case "ast2 exposes module type declaration tokens" test_module_type_declaration_tokens;
  Test.case "ast2 exposes let binding type annotation views" test_binding_type_annotation_view;
  Test.case "ast2 exposes record expression and pattern views" test_record_views;
  Test.case "ast2 exposes binding operator expression views" test_binding_operator_views;
  Test.case "ast2 exposes local open expression and pattern views" test_local_open_views;
  Test.case "ast2 exposes first-class module expression views" test_first_class_module_views;
  Test.case "ast2 exposes let module expression views" test_let_module_expression_views;
]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"syn-ast2" ~tests ~args ()) ~args:Env.args ()
