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

let test_labeled_application_after_poly_variant_argument = fun _ctx ->
  let source = "let parse_interface ~source tokens = parse ~cst_kind:`Interface \
     ~parse_item:parse_signature_item ~source ~tokens\n"
  in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let item = nth_structure_item root 0 |> require_some ~msg:"expected first structure item" in
  let body = item
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected let binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected binding body" in
  let rec application_parts expr args =
    match Ast2.Expr.view expr with
    | Ast2.Expr.Apply { callee=Some callee; argument=Some argument } -> application_parts
      callee
      (argument :: args)
    | _ -> (expr, args)
  in
  let assert_labeled_arg arg expected =
    match Ast2.Expr.view arg with
    | Ast2.Expr.LabeledArg { label=Some label; value } ->
        Test.assert_equal ~expected ~actual:(Ast2.Token.text label);
        value
    | _ -> panic ("expected labeled argument " ^ expected)
  in
  let callee, arguments = application_parts body [] in
  (
    match Ast2.Expr.view callee with
    | Ast2.Expr.Path { path } -> assert_last_ident_text path "parse"
    | _ -> panic "expected parse callee"
  );
  match arguments with
  | [cst_kind;parse_item;source;tokens] ->
      let cst_kind_value = assert_labeled_arg cst_kind "cst_kind" in
      (
        match cst_kind_value |> require_some ~msg:"expected cst_kind value" |> Ast2.Expr.view with
        | Ast2.Expr.PolyVariant { payload=None } -> ()
        | _ -> panic "expected cst_kind value to be a bare polymorphic variant"
      );
      ignore (assert_labeled_arg parse_item "parse_item");
      ignore (assert_labeled_arg source "source");
      ignore (assert_labeled_arg tokens "tokens");
      Ok ()
  | _ -> Error "expected parse application to have four labeled arguments"

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

let test_type_tuple_separator_views = fun _ctx ->
  let root = parse_mli "type ('a, 'e) result_like = ('a, 'e) result\ntype pair = int * string\n"
  |> Result.expect ~msg:"expected parse2 interface" in
  let result_item = nth_signature_item root 0 |> require_some ~msg:"expected result-like type item" in
  (
    match Ast2.SignatureItem.view result_item with
    | Ast2.SignatureItem.Type decl ->
        let manifest = Ast2.TypeDeclaration.manifest decl |> require_some ~msg:"expected result-like manifest" in
        (
          match Ast2.TypeExpr.view manifest with
          | Ast2.TypeExpr.Apply { argument=Some argument; constructor=Some constructor } ->
              assert_type_path_last_ident constructor "result";
              (
                match Ast2.TypeExpr.view argument with
                | Ast2.TypeExpr.Tuple {
                  separator=Ast2.TypeExpr.Comma;
                  left=Some left;
                  right=Some right
                } -> (
                    match Ast2.TypeExpr.view left, Ast2.TypeExpr.view right with
                    | Ast2.TypeExpr.Var { name=Some left_name }, Ast2.TypeExpr.Var {
                      name=Some right_name
                    } ->
                        Test.assert_equal ~expected:"a" ~actual:(Ast2.Token.text left_name);
                        Test.assert_equal ~expected:"e" ~actual:(Ast2.Token.text right_name);
                        Ok ()
                    | _ -> Error "expected comma tuple type variables"
                  )
                | _ -> Error "expected comma tuple type argument"
              )
          | _ -> Error "expected type constructor application"
        )
    | _ -> Error "expected result-like type declaration"
  ) |> Result.expect ~msg:"expected comma type tuple";
  let pair_item = nth_signature_item root 1 |> require_some ~msg:"expected pair type item" in
  match Ast2.SignatureItem.view pair_item with
  | Ast2.SignatureItem.Type decl ->
      let manifest = Ast2.TypeDeclaration.manifest decl |> require_some ~msg:"expected pair manifest" in
      (
        match Ast2.TypeExpr.view manifest with
        | Ast2.TypeExpr.Tuple { separator=Ast2.TypeExpr.Star; left=Some left; right=Some right } ->
            assert_type_path_last_ident left "int";
            assert_type_path_last_ident right "string";
            Ok ()
        | _ -> Error "expected star tuple type"
      )
  | _ -> Error "expected pair type declaration"

let test_poly_labeled_and_signed_views = fun _ctx ->
  let source = "let make:\n  type socket err. reader:(socket, err) reader -> t = fun ~reader -> value\n\
                let f = function | -1 -> true | +2 -> false\n"
  in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let make_binding = nth_structure_item root 0
  |> require_some ~msg:"expected make item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected make binding" in
  let annotation = Ast2.LetBinding.type_annotation make_binding |> require_some ~msg:"expected make type annotation" in
  (
    match Ast2.TypeExpr.view annotation with
    | Ast2.TypeExpr.Poly { body=Some body } ->
        let names = ref [] in
        Ast2.TypeExpr.for_each_poly_type_name
          annotation
          ~fn:(fun token -> names := Ast2.Token.text token :: !names);
        Test.assert_equal ~expected:[ "socket"; "err" ] ~actual:(List.reverse !names);
        (
          match Ast2.TypeExpr.view body with
          | Ast2.TypeExpr.Arrow { left=Some left; right=Some _ } -> (
              match Ast2.TypeExpr.view left with
              | Ast2.TypeExpr.Labeled { label=Some label; annotation=Some _; _ } -> Test.assert_equal
                ~expected:"reader"
                ~actual:(Ast2.Token.text label)
              | _ -> panic "expected labeled arrow argument type"
            )
          | _ -> panic "expected poly type arrow body"
        )
    | _ -> panic "expected poly type annotation"
  );
  let function_body = nth_structure_item root 1
  |> require_some ~msg:"expected function item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected function binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected function body" in
  match Ast2.Expr.view function_body with
  | Ast2.Expr.Function { first_case=Some first_case } -> (
      let case = Ast2.MatchCase.view first_case in
      let pattern = case.Ast2.MatchCase.pattern |> require_some ~msg:"expected first function case pattern" in
      match Ast2.Pattern.view pattern with
      | Ast2.Pattern.Literal { token=Some token } ->
          let sign = Ast2.Pattern.literal_sign_token pattern |> require_some ~msg:"expected signed literal sign" in
          Test.assert_equal ~expected:"-" ~actual:(Ast2.Token.text sign);
          Test.assert_equal ~expected:"1" ~actual:(Ast2.Token.text token);
          Ok ()
      | _ -> Error "expected signed literal pattern"
    )
  | _ -> Error "expected function expression"

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

let test_type_declaration_member_views = fun _ctx ->
  let root = parse_mli "type 'a box = 'a list and color = Red | Blue and point = { x : int }\n"
  |> Result.expect ~msg:"expected parse2 interface" in
  let type_item = nth_signature_item root 0 |> require_some ~msg:"expected type signature item" in
  match Ast2.SignatureItem.view type_item with
  | Ast2.SignatureItem.Type decl ->
      let count = ref 0 in
      let names = ref [] in
      let shells = ref [] in
      let parameter_counts = ref [] in
      let manifest_shapes = ref [] in
      Ast2.TypeDeclaration.for_each_member decl
        ~fn:(fun member ->
          count := !count + 1;
          let name = Ast2.TypeDeclaration.Member.name member |> require_some ~msg:"expected type member name" in
          let shell = Ast2.TypeDeclaration.Member.shell_token member |> require_some ~msg:"expected type member shell" in
          let parameters = ref 0 in
          Ast2.TypeDeclaration.Member.for_each_parameter
            member
            ~fn:(fun _ -> parameters := !parameters + 1);
          let has_manifest =
            match Ast2.TypeDeclaration.Member.manifest member with
            | Some _ -> true
            | None -> false
          in
          names := Ast2.Token.text name :: !names;
          shells := Ast2.Token.text shell :: !shells;
          parameter_counts := !parameters :: !parameter_counts;
          manifest_shapes := has_manifest :: !manifest_shapes);
      Test.assert_equal ~expected:3 ~actual:!count;
      Test.assert_equal ~expected:[ "box"; "color"; "point" ] ~actual:(List.reverse !names);
      Test.assert_equal ~expected:[ "type"; "and"; "and" ] ~actual:(List.reverse !shells);
      Test.assert_equal ~expected:[ 1; 0; 0 ] ~actual:(List.reverse !parameter_counts);
      Test.assert_equal ~expected:[ true; false; false ] ~actual:(List.reverse !manifest_shapes);
      Ok ()
  | _ -> Error "expected type declaration"

let test_type_declaration_body_group_views = fun _ctx ->
  let root =
    parse_mli
      "type color = Red | Blue of int | Pair of int * string\n\
     type point = private { mutable x : int; y : string }\n"
    |> Result.expect ~msg:"expected parse2 interface"
  in
  let color_item = nth_signature_item root 0 |> require_some ~msg:"expected color type item" in
  (
    match Ast2.SignatureItem.view color_item with
    | Ast2.SignatureItem.Type decl ->
        let member =
          Ast2.TypeDeclaration.fold_members decl None
            (fun acc member ->
              match acc with
              | Some _ -> acc
              | None -> Some member)
          |> require_some ~msg:"expected color type member"
        in
        let variant = Ast2.TypeDeclaration.Member.variant_type member |> require_some ~msg:"expected variant type body" in
        let names = ref [] in
        let pipe_flags = ref [] in
        let payload_shapes = ref [] in
        Ast2.VariantType.for_each_constructor variant
          ~fn:(fun constructor ->
            let name = Ast2.VariantConstructor.name constructor |> require_some ~msg:"expected constructor name" in
            names := Ast2.Token.text name :: !names;
            pipe_flags := Option.is_some (Ast2.VariantConstructor.pipe_token constructor) :: !pipe_flags;
            let payload_shape =
              match Ast2.VariantConstructor.payload_type constructor with
              | None -> "none"
              | Some payload -> (
                  match Ast2.TypeExpr.view payload with
                  | Ast2.TypeExpr.Path _ -> "path"
                  | Ast2.TypeExpr.Tuple { separator=Ast2.TypeExpr.Star; _ } -> "tuple"
                  | _ -> "other"
                )
            in
            payload_shapes := payload_shape :: !payload_shapes);
        Test.assert_equal ~expected:[ "Red"; "Blue"; "Pair" ] ~actual:(List.reverse !names);
        Test.assert_equal ~expected:[ false; true; true ] ~actual:(List.reverse !pipe_flags);
        Test.assert_equal
          ~expected:[ "none"; "path"; "tuple" ]
          ~actual:(List.reverse !payload_shapes);
        Ok ()
    | _ -> Error "expected color type declaration"
  ) |> Result.expect ~msg:"expected variant type body";
  let point_item = nth_signature_item root 1 |> require_some ~msg:"expected point type item" in
  match Ast2.SignatureItem.view point_item with
  | Ast2.SignatureItem.Type decl ->
      let member =
        Ast2.TypeDeclaration.fold_members decl None
          (fun acc member ->
            match acc with
            | Some _ -> acc
            | None -> Some member)
        |> require_some ~msg:"expected point type member"
      in
      let record = Ast2.TypeDeclaration.Member.record_type member |> require_some ~msg:"expected record type body" in
      Test.assert_true (Option.is_some (Ast2.RecordType.private_token record));
      let names = ref [] in
      let mutable_flags = ref [] in
      let field_types = ref [] in
      Ast2.RecordType.for_each_field record
        ~fn:(fun field ->
          let name = Ast2.RecordField.name field |> require_some ~msg:"expected record field name" in
          names := Ast2.Token.text name :: !names;
          mutable_flags := Option.is_some (Ast2.RecordField.mutable_token field) :: !mutable_flags;
          let annotation = Ast2.RecordField.type_annotation field |> require_some ~msg:"expected record field type" in
          (
            match Ast2.TypeExpr.view annotation with
            | Ast2.TypeExpr.Path { path } ->
                let last = Ast2.Path.last_ident path |> require_some ~msg:"expected field type path" in
                field_types := Ast2.Token.text last :: !field_types
            | _ -> panic "expected field type path"
          ));
      Test.assert_equal ~expected:[ "x"; "y" ] ~actual:(List.reverse !names);
      Test.assert_equal ~expected:[ true; false ] ~actual:(List.reverse !mutable_flags);
      Test.assert_equal ~expected:[ "int"; "string" ] ~actual:(List.reverse !field_types);
      Ok ()
  | _ -> Error "expected point type declaration"

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
      let child_kinds = ref [] in
      let name = Ast2.ExceptionDeclaration.name decl |> require_some ~msg:"expected exception name" in
      Ast2.Node.for_each_child_node
        decl
        ~fn:(fun child -> child_kinds := Ast2.Node.kind child :: !child_kinds);
      Test.assert_equal ~expected:"Boom" ~actual:(Ast2.Token.text name);
      Test.assert_equal
        ~expected:[ SyntaxKind2.EXCEPTION_DECL_HEAD ]
        ~actual:(List.reverse !child_kinds);
      Ok ()
  | _ -> Error "expected exception declaration"

let test_type_extension_and_exception_views = fun _ctx ->
  let source = "type 'a box += | More of 'a\nexception Parse_error of string\nexception Nested = Std.Result.Error\n" in
  let root = parse_mli source |> Result.expect ~msg:"expected parse2 interface" in
  let type_extension_item = nth_signature_item root 0 |> require_some ~msg:"expected type extension item" in
  (
    match Ast2.SignatureItem.view type_extension_item with
    | Ast2.SignatureItem.TypeExtension decl ->
        let child_kinds = ref [] in
        Ast2.Node.for_each_child_node
          decl
          ~fn:(fun child -> child_kinds := Ast2.Node.kind child :: !child_kinds);
        let name = Ast2.TypeExtensionDeclaration.name decl |> require_some ~msg:"expected type extension name" in
        Test.assert_equal ~expected:"box" ~actual:(Ast2.Token.text name);
        Test.assert_equal
          ~expected:[ SyntaxKind2.TYPE_EXTENSION_DECL_HEAD; SyntaxKind2.TYPE_EXTENSION_DECL_BODY ]
          ~actual:(List.reverse !child_kinds);
        let parameter_count = ref 0 in
        Ast2.TypeExtensionDeclaration.for_each_parameter
          decl
          ~fn:(fun _ -> parameter_count := !parameter_count + 1);
        Test.assert_equal ~expected:1 ~actual:!parameter_count;
        let variant = Ast2.TypeExtensionDeclaration.variant_type decl |> require_some ~msg:"expected type extension body" in
        let constructor = ref None in
        Ast2.VariantType.for_each_constructor variant
          ~fn:(fun current ->
            match !constructor with
            | Some _ -> ()
            | None -> constructor := Some current);
        let constructor = !constructor |> require_some ~msg:"expected type extension constructor" in
        let constructor_name = Ast2.VariantConstructor.name constructor |> require_some ~msg:"expected type extension constructor name" in
        Test.assert_equal ~expected:"More" ~actual:(Ast2.Token.text constructor_name);
        (
          match Ast2.VariantConstructor.payload_type constructor with
          | Some payload -> (
              match Ast2.TypeExpr.view payload with
              | Ast2.TypeExpr.Var { name=Some payload_name } -> Test.assert_equal
                ~expected:"a"
                ~actual:(Ast2.Token.text payload_name)
              | _ -> panic "expected type extension payload type variable"
            )
          | None -> panic "expected type extension payload"
        );
        Ok ()
    | _ -> Error "expected type extension declaration"
  ) |> Result.expect ~msg:"expected type extension view";
  let payload_item = nth_signature_item root 1 |> require_some ~msg:"expected exception payload item" in
  (
    match Ast2.SignatureItem.view payload_item with
    | Ast2.SignatureItem.Exception decl ->
        let name = Ast2.ExceptionDeclaration.name decl |> require_some ~msg:"expected exception payload name" in
        Test.assert_equal ~expected:"Parse_error" ~actual:(Ast2.Token.text name);
        (
          match Ast2.ExceptionDeclaration.view decl with
          | Ast2.ExceptionDeclaration.Payload {
            of_token=Some of_token;
            payload=Some (TypeExpr payload)
          } ->
              Test.assert_equal ~expected:"of" ~actual:(Ast2.Token.text of_token);
              assert_type_path_last_ident payload "string";
              Ok ()
          | _ -> Error "expected exception payload view"
        )
    | _ -> Error "expected exception declaration"
  ) |> Result.expect ~msg:"expected exception payload view";
  let alias_item = nth_signature_item root 2 |> require_some ~msg:"expected exception alias item" in
  match Ast2.SignatureItem.view alias_item with
  | Ast2.SignatureItem.Exception decl ->
      let name = Ast2.ExceptionDeclaration.name decl |> require_some ~msg:"expected exception alias name" in
      Test.assert_equal ~expected:"Nested" ~actual:(Ast2.Token.text name);
      (
        match Ast2.ExceptionDeclaration.view decl with
        | Ast2.ExceptionDeclaration.Alias { equals_token=Some equals_token; path=Some path } ->
            Test.assert_equal ~expected:"=" ~actual:(Ast2.Token.text equals_token);
            assert_last_ident_text path "Error";
            Ok ()
        | _ -> Error "expected exception alias view"
      )
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

let test_module_declaration_member_views = fun _ctx ->
  let root = parse_ml "module rec M : S = A and N : T = B\n" |> Result.expect ~msg:"expected parse2 source file" in
  let item = nth_structure_item root 0 |> require_some ~msg:"expected module item" in
  match Ast2.StructureItem.view item with
  | Ast2.StructureItem.Module decl ->
      Test.assert_equal ~expected:true ~actual:(Ast2.ModuleDeclaration.is_recursive decl);
      let count = ref 0 in
      let names = ref [] in
      let shells = ref [] in
      let body_shapes = ref [] in
      Ast2.ModuleDeclaration.for_each_member decl
        ~fn:(fun member ->
          count := !count + 1;
          let name = Ast2.ModuleDeclaration.Member.name member |> require_some ~msg:"expected member name" in
          let shell = Ast2.ModuleDeclaration.Member.child_token_at member 0 |> require_some ~msg:"expected member shell token" in
          let has_module_type =
            match Ast2.ModuleDeclaration.Member.module_type member with
            | Some _ -> true
            | None -> false
          in
          let has_module_expr =
            match Ast2.ModuleDeclaration.Member.module_expr member with
            | Some _ -> true
            | None -> false
          in
          names := Ast2.Token.text name :: !names;
          shells := Ast2.Token.text shell :: !shells;
          body_shapes := (has_module_type, has_module_expr) :: !body_shapes);
      Test.assert_equal ~expected:2 ~actual:!count;
      Test.assert_equal ~expected:[ "M"; "N" ] ~actual:(List.reverse !names);
      Test.assert_equal ~expected:[ "module"; "and" ] ~actual:(List.reverse !shells);
      Test.assert_equal ~expected:[ (true, true); (true, true) ] ~actual:(List.reverse !body_shapes);
      Ok ()
  | _ -> Error "expected module declaration"

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
        let child_kinds = ref [] in
        let segments = ref [] in
        Ast2.Node.for_each_child_node
          decl
          ~fn:(fun child -> child_kinds := Ast2.Node.kind child :: !child_kinds);
        Ast2.ModuleTypeDeclaration.for_each_body_path_ident
          decl
          ~fn:(fun token -> segments := Ast2.Token.text token :: !segments);
        Test.assert_equal ~expected:"S" ~actual:(Ast2.Token.text name);
        Test.assert_equal ~expected:"=" ~actual:(Ast2.Token.text equals);
        Test.assert_equal
          ~expected:[ SyntaxKind2.MODULE_TYPE_DECL_HEAD; SyntaxKind2.MODULE_TYPE_DECL_BODY ]
          ~actual:(List.reverse !child_kinds);
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

let test_module_type_with_constraint_views = fun _ctx ->
  let root = parse_mli "module type S = Driver with type config = int and module Nested = Impl\n"
  |> Result.expect ~msg:"expected parse2 interface" in
  let item = nth_signature_item root 0 |> require_some ~msg:"expected module type item" in
  match Ast2.SignatureItem.view item with
  | Ast2.SignatureItem.ModuleType decl ->
      Test.assert_equal
        ~expected:Ast2.ModuleTypeDeclaration.With
        ~actual:(Ast2.ModuleTypeDeclaration.body decl);
      (
        match Ast2.ModuleTypeDeclaration.base_module_type decl with
        | Some base ->
            let path = Ast2.Path.cast base |> require_some ~msg:"expected constrained base path" in
            let name = Ast2.Path.last_ident path |> require_some ~msg:"expected constrained base name" in
            Test.assert_equal ~expected:"Driver" ~actual:(Ast2.Token.text name)
        | None -> panic "expected constrained base module type"
      );
      let seen = ref 0 in
      Ast2.ModuleTypeDeclaration.for_each_constraint decl
        ~fn:(fun constraint_ ->
          let index = !seen in
          seen := !seen + 1;
          match Ast2.ModuleTypeConstraint.view constraint_ with
          | Ast2.ModuleTypeConstraint.Type { path; operator; body } when Int.equal index 0 ->
              let path = path |> require_some ~msg:"expected type constraint path" in
              let operator = operator |> require_some ~msg:"expected type constraint operator" in
              let body = body |> require_some ~msg:"expected type constraint body" in
              let path_name = Ast2.Path.last_ident path |> require_some ~msg:"expected type path name" in
              let body_token = Ast2.Node.first_descendant_token body |> require_some ~msg:"expected type constraint body token" in
              Test.assert_equal ~expected:"config" ~actual:(Ast2.Token.text path_name);
              Test.assert_equal ~expected:"=" ~actual:(Ast2.Token.text operator);
              Test.assert_equal ~expected:"int" ~actual:(Ast2.Token.text body_token)
          | Ast2.ModuleTypeConstraint.Module { path; body } when Int.equal index 1 ->
              let path = path |> require_some ~msg:"expected module constraint path" in
              let body = body |> require_some ~msg:"expected module constraint body" in
              let path_name = Ast2.Path.last_ident path |> require_some ~msg:"expected module path name" in
              let body_token = Ast2.Node.first_descendant_token body |> require_some ~msg:"expected module constraint body token" in
              Test.assert_equal ~expected:"Nested" ~actual:(Ast2.Token.text path_name);
              Test.assert_equal ~expected:"Impl" ~actual:(Ast2.Token.text body_token)
          | Ast2.ModuleTypeConstraint.Unknown _ ->
              panic "unexpected module type constraint shape"
          | _ ->
              panic "unexpected module type constraint ordering");
      Test.assert_equal ~expected:2 ~actual:!seen;
      Ok ()
  | _ -> Error "expected module type declaration"

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

let first_class_module_pattern_ascription_text = fun pattern ->
  let segments = ref [] in
  Ast2.FirstClassModulePattern.for_each_ascription_path_ident
    pattern
    ~fn:(fun token -> segments := Ast2.Token.text token :: !segments);
  List.reverse !segments |> String.concat "."

let let_module_body_path_text = fun expr ->
  let segments = ref [] in
  Ast2.LetModuleExpr.for_each_module_body_path_ident
    expr
    ~fn:(fun token -> segments := Ast2.Token.text token :: !segments);
  List.reverse !segments |> String.concat "."

let let_exception_payload_tokens = fun expr ->
  let tokens = ref [] in
  Ast2.LetExceptionExpr.for_each_payload_token
    expr
    ~fn:(fun token -> tokens := Ast2.Token.text token :: !tokens);
  List.reverse !tokens

let test_local_open_views = fun _ctx ->
  let source = "let value = let open Foo.Bar in result\nlet Foo.Bar.(x) = value\nlet Frame.{ payload } = frame\n" in
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
  );
  let local_open_record_pattern = nth_structure_item root 2
  |> require_some ~msg:"expected local open record pattern item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected local open record pattern binding"
  |> pattern_of_binding
  |> Result.expect ~msg:"expected local open record pattern" in
  let local_open_record_pattern = Ast2.LocalOpenPattern.cast local_open_record_pattern
  |> require_some ~msg:"expected local open record pattern view" in
  Test.assert_equal
    ~expected:"Frame"
    ~actual:(local_open_pattern_path_text local_open_record_pattern);
  let opening = Ast2.LocalOpenPattern.opening_token local_open_record_pattern |> require_some ~msg:"expected local open record opening" in
  let closing = Ast2.LocalOpenPattern.closing_token local_open_record_pattern |> require_some ~msg:"expected local open record closing" in
  Test.assert_equal ~expected:"{" ~actual:(Ast2.Token.text opening);
  Test.assert_equal ~expected:"}" ~actual:(Ast2.Token.text closing);
  match Ast2.LocalOpenPattern.pattern local_open_record_pattern with
  | Some pattern -> (
      match Ast2.Pattern.view pattern with
      | Ast2.Pattern.Record -> Ok ()
      | _ -> Error "expected local open record inner pattern"
    )
  | None -> Error "expected local open record inner pattern"

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

let test_let_exception_expression_views = fun _ctx ->
  let source = "let value = let exception Local of int * Foo.t in result\nlet bare = let exception Done in done_\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let value_expr = nth_structure_item root 0
  |> require_some ~msg:"expected let exception item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected outer binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected let exception body" in
  let exception_expr = Ast2.LetExceptionExpr.cast value_expr |> require_some ~msg:"expected let exception expression view" in
  let exception_token = Ast2.LetExceptionExpr.exception_token exception_expr |> require_some ~msg:"expected exception token" in
  let name = Ast2.LetExceptionExpr.name exception_expr |> require_some ~msg:"expected exception name" in
  let of_token = Ast2.LetExceptionExpr.of_token exception_expr |> require_some ~msg:"expected of token" in
  let in_token = Ast2.LetExceptionExpr.in_token exception_expr |> require_some ~msg:"expected in token" in
  Test.assert_equal ~expected:"exception" ~actual:(Ast2.Token.text exception_token);
  Test.assert_equal ~expected:"Local" ~actual:(Ast2.Token.text name);
  Test.assert_equal ~expected:"of" ~actual:(Ast2.Token.text of_token);
  Test.assert_equal ~expected:"in" ~actual:(Ast2.Token.text in_token);
  Test.assert_equal
    ~expected:[ "int"; "*"; "Foo"; "."; "t" ]
    ~actual:(let_exception_payload_tokens exception_expr);
  (
    match Ast2.LetExceptionExpr.body exception_expr with
    | Some body -> (
        match Ast2.Expr.view body with
        | Ast2.Expr.Path { path } -> Test.assert_equal
          ~expected:"result"
          ~actual:(last_path_text path)
        | _ -> panic "expected let exception body path"
      )
    | None -> panic "expected let exception expression body"
  );
  let bare_expr = nth_structure_item root 1
  |> require_some ~msg:"expected bare let exception item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected bare outer binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected bare let exception body" in
  let bare_exception = Ast2.LetExceptionExpr.cast bare_expr |> require_some ~msg:"expected bare let exception view" in
  (
    match Ast2.LetExceptionExpr.of_token bare_exception with
    | None -> Ok ()
    | Some _ -> Error "expected bare let exception without payload"
  )

let test_unreachable_expression_views = fun _ctx ->
  let source = "let value = match maybe with | Some value -> value | None -> .\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let match_expr = nth_structure_item root 0
  |> require_some ~msg:"expected let item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected outer binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected match body" in
  let unreachable = ref None in
  Ast2.Expr.for_each_match_case match_expr
    ~fn:(fun match_case ->
      let view = Ast2.MatchCase.view match_case in
      match view.body with
      | Some body -> (
          match Ast2.Expr.view body with
          | Ast2.Expr.Unreachable -> unreachable := Some body
          | _ -> ()
        )
      | None -> ());
  let unreachable = !unreachable |> require_some ~msg:"expected unreachable expression" in
  let unreachable = Ast2.UnreachableExpr.cast unreachable |> require_some ~msg:"expected unreachable expression view" in
  let dot = Ast2.UnreachableExpr.dot_token unreachable |> require_some ~msg:"expected dot token" in
  Test.assert_equal ~expected:"." ~actual:(Ast2.Token.text dot);
  Ok ()

let attribute_shell_text = fun ~for_each_shell_token ->
  let text = ref "" in
  let first = ref true in
  for_each_shell_token
    ~fn:(fun token ->
      if !first then
        (
          first := false;
          text := !text ^ Ast2.Token.text token
        )
      else
        text := !text ^ Ast2.Token.full_text token);
  !text

let test_attribute_views = fun _ctx ->
  let source = "let value = target [@inline always]\nlet (x [@foo]) = value\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let attribute_expr = nth_structure_item root 0
  |> require_some ~msg:"expected attribute expression item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected attribute expression binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected attribute expression body" in
  (
    match Ast2.Expr.view attribute_expr with
    | Ast2.Expr.Attribute { inner=Some inner } -> (
        match Ast2.Expr.view inner with
        | Ast2.Expr.Path { path } -> assert_last_ident_text path "target"
        | _ -> panic "expected attributed expression inner path"
      )
    | _ -> panic "expected attribute expression"
  );
  let attribute_expr = Ast2.AttributeExpr.cast attribute_expr |> require_some ~msg:"expected attribute expression view" in
  Test.assert_equal
    ~expected:"[@inline always]"
    ~actual:(attribute_shell_text
      ~for_each_shell_token:(fun ~fn -> Ast2.AttributeExpr.for_each_shell_token attribute_expr ~fn));
  let attribute_pattern = nth_structure_item root 1
  |> require_some ~msg:"expected attribute pattern item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected attribute pattern binding"
  |> pattern_of_binding
  |> Result.expect ~msg:"expected attribute pattern" in
  let attribute_pattern =
    match Ast2.Pattern.view attribute_pattern with
    | Ast2.Pattern.Parenthesized { inner=Some inner } -> inner
    | _ -> panic "expected parenthesized attribute pattern"
  in
  let attribute_pattern = Ast2.AttributePattern.cast attribute_pattern |> require_some ~msg:"expected attribute pattern view" in
  (
    match Ast2.AttributePattern.inner attribute_pattern with
    | Some inner -> (
        match Ast2.Pattern.view inner with
        | Ast2.Pattern.Path { path } -> assert_last_ident_text path "x"
        | _ -> panic "expected attributed pattern inner path"
      )
    | None -> panic "expected attribute pattern inner"
  );
  Test.assert_equal
    ~expected:"[@foo]"
    ~actual:(attribute_shell_text
      ~for_each_shell_token:(fun ~fn ->
        Ast2.AttributePattern.for_each_shell_token attribute_pattern ~fn));
  Ok ()

let test_extension_views = fun _ctx ->
  let source = "let value = [%expr payload]\nlet [%pat payload] = value\n[%%item payload]\n[@@@warning \"-32\"]\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let extension_expr = nth_structure_item root 0
  |> require_some ~msg:"expected extension expression item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected extension expression binding"
  |> body_of_binding
  |> Result.expect ~msg:"expected extension expression body" in
  (
    match Ast2.Expr.view extension_expr with
    | Ast2.Expr.Extension -> ()
    | _ -> panic "expected extension expression"
  );
  let extension_expr = Ast2.ExtensionExpr.cast extension_expr |> require_some ~msg:"expected extension expression view" in
  Test.assert_equal
    ~expected:"[%expr payload]"
    ~actual:(attribute_shell_text
      ~for_each_shell_token:(fun ~fn -> Ast2.ExtensionExpr.for_each_shell_token extension_expr ~fn));
  let extension_pattern = nth_structure_item root 1
  |> require_some ~msg:"expected extension pattern item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected extension pattern binding"
  |> pattern_of_binding
  |> Result.expect ~msg:"expected extension pattern" in
  (
    match Ast2.Pattern.view extension_pattern with
    | Ast2.Pattern.Extension -> ()
    | _ -> panic "expected extension pattern"
  );
  let extension_pattern = Ast2.ExtensionPattern.cast extension_pattern |> require_some ~msg:"expected extension pattern view" in
  Test.assert_equal
    ~expected:"[%pat payload]"
    ~actual:(attribute_shell_text
      ~for_each_shell_token:(fun ~fn ->
        Ast2.ExtensionPattern.for_each_shell_token extension_pattern ~fn));
  let extension_item = nth_structure_item root 2 |> require_some ~msg:"expected extension item" in
  (
    match Ast2.StructureItem.view extension_item with
    | Ast2.StructureItem.Extension item -> Test.assert_equal
      ~expected:"[%%item payload]"
      ~actual:(attribute_shell_text
        ~for_each_shell_token:(fun ~fn -> Ast2.ExtensionItem.for_each_shell_token item ~fn))
    | _ -> panic "expected extension structure item"
  );
  let attribute_item = nth_structure_item root 3 |> require_some ~msg:"expected attribute item" in
  (
    match Ast2.StructureItem.view attribute_item with
    | Ast2.StructureItem.Attribute item -> Test.assert_equal
      ~expected:"[@@@warning \"-32\"]"
      ~actual:(attribute_shell_text
        ~for_each_shell_token:(fun ~fn -> Ast2.AttributeItem.for_each_shell_token item ~fn))
    | _ -> panic "expected attribute structure item"
  );
  Ok ()

let test_special_pattern_views = fun _ctx ->
  let source = "let f (type a b) (module M : S.T) = value\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let binding = nth_structure_item root 0
  |> require_some ~msg:"expected special pattern item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected special pattern binding" in
  let parameters = ref [] in
  Ast2.LetBinding.for_each_parameter
    binding
    ~fn:(fun pattern -> parameters := pattern :: !parameters);
  match List.reverse !parameters with
  | [locally_abstract;first_class_module] ->
      (
        match Ast2.Pattern.view locally_abstract with
        | Ast2.Pattern.LocallyAbstractType -> ()
        | _ -> panic "expected locally abstract type pattern"
      );
      let locally_abstract = Ast2.LocallyAbstractTypePattern.cast locally_abstract
      |> require_some ~msg:"expected locally abstract type pattern view" in
      let type_names = ref [] in
      Ast2.LocallyAbstractTypePattern.for_each_type_name
        locally_abstract
        ~fn:(fun token -> type_names := Ast2.Token.text token :: !type_names);
      Test.assert_equal ~expected:[ "a"; "b" ] ~actual:(List.reverse !type_names);
      (
        match Ast2.Pattern.view first_class_module with
        | Ast2.Pattern.FirstClassModule -> ()
        | _ -> panic "expected first-class module pattern"
      );
      let first_class_module = Ast2.FirstClassModulePattern.cast first_class_module
      |> require_some ~msg:"expected first-class module pattern view" in
      let binder = Ast2.FirstClassModulePattern.binder first_class_module |> require_some ~msg:"expected first-class module binder" in
      let colon = Ast2.FirstClassModulePattern.colon_token first_class_module |> require_some ~msg:"expected first-class module colon" in
      Test.assert_equal ~expected:"M" ~actual:(Ast2.Token.text binder);
      Test.assert_equal ~expected:":" ~actual:(Ast2.Token.text colon);
      Test.assert_equal
        ~expected:Ast2.FirstClassModulePattern.PathAscription
        ~actual:(Ast2.FirstClassModulePattern.ascription first_class_module);
      Test.assert_equal
        ~expected:"S.T"
        ~actual:(first_class_module_pattern_ascription_text first_class_module);
      Ok ()
  | _ -> Error "expected two special-pattern parameters"

let test_typed_labeled_parameter_view = fun _ctx ->
  let source = "let map (type a b) (iter : a t) ~(fn : a -> b) = ()\n" in
  let root = parse_ml source |> Result.expect ~msg:"expected parse2 source file" in
  let binding = nth_structure_item root 0
  |> require_some ~msg:"expected typed labeled parameter item"
  |> binding_of_structure_item
  |> Result.expect ~msg:"expected typed labeled parameter binding" in
  let parameters = ref [] in
  Ast2.LetBinding.for_each_parameter
    binding
    ~fn:(fun pattern -> parameters := pattern :: !parameters);
  match List.reverse !parameters with
  | [_locally_abstract;_iter;labeled] -> (
      match Ast2.Pattern.view labeled with
      | Ast2.Pattern.LabeledParam parameter -> (
          match Ast2.Parameter.view parameter with
          | Ast2.Parameter.Labeled { label=Some label; pattern=Some pattern } ->
              Test.assert_equal ~expected:"fn" ~actual:(Ast2.Token.text label);
              (
                match Ast2.Pattern.view pattern with
                | Ast2.Pattern.Constraint { pattern=Some binding; annotation=Some annotation } ->
                    (
                      match Ast2.Pattern.view binding with
                      | Ast2.Pattern.Path { path } -> assert_last_ident_text path "fn"
                      | _ -> panic "expected labeled parameter binding path"
                    );
                    (
                      match Ast2.TypeExpr.view annotation with
                      | Ast2.TypeExpr.Arrow { left=Some _; right=Some _ } -> Ok ()
                      | _ -> Error "expected labeled parameter arrow annotation"
                    )
                | _ -> Error "expected typed labeled parameter pattern"
              )
          | _ -> Error "expected labeled parameter view with label and typed pattern"
        )
      | _ -> Error "expected labeled parameter pattern"
    )
  | _ -> Error "expected locally abstract, positional, and labeled parameters"

let tests = [
  Test.case "ast2 exposes source file and let binding views" test_source_file_and_let_binding_views;
  Test.case "ast2 exposes if and match expression views" test_expression_views;
  Test.case "ast2 keeps labels after polymorphic variant arguments as application arguments" test_labeled_application_after_poly_variant_argument;
  Test.case "ast2 exposes tuple and cons pattern views" test_pattern_views;
  Test.case "ast2 exposes signature declaration views" test_signature_and_type_views;
  Test.case "ast2 exposes type expression views" test_type_expression_views;
  Test.case "ast2 exposes type tuple separators" test_type_tuple_separator_views;
  Test.case "ast2 exposes poly labeled types and signed literal patterns" test_poly_labeled_and_signed_views;
  Test.case "ast2 keeps non-manifest type bodies out of manifest views" test_non_manifest_type_declaration_bodies;
  Test.case "ast2 exposes type declaration parameters" test_type_declaration_parameters;
  Test.case "ast2 exposes type declaration member views" test_type_declaration_member_views;
  Test.case "ast2 exposes type declaration body group views" test_type_declaration_body_group_views;
  Test.case "ast2 exposes type extensions and structured exception views" test_type_extension_and_exception_views;
  Test.case "ast2 exposes open declaration path tokens" test_open_declaration_path_tokens;
  Test.case "ast2 exposes simple declaration token views" test_simple_declaration_token_views;
  Test.case "ast2 exposes module declaration tokens" test_module_declaration_tokens;
  Test.case "ast2 exposes module declaration member views" test_module_declaration_member_views;
  Test.case "ast2 exposes module type declaration tokens" test_module_type_declaration_tokens;
  Test.case "ast2 exposes module type with-constraint views" test_module_type_with_constraint_views;
  Test.case "ast2 exposes let binding type annotation views" test_binding_type_annotation_view;
  Test.case "ast2 exposes record expression and pattern views" test_record_views;
  Test.case "ast2 exposes binding operator expression views" test_binding_operator_views;
  Test.case "ast2 exposes local open expression and pattern views" test_local_open_views;
  Test.case "ast2 exposes first-class module expression views" test_first_class_module_views;
  Test.case "ast2 exposes let module expression views" test_let_module_expression_views;
  Test.case "ast2 exposes let exception expression views" test_let_exception_expression_views;
  Test.case "ast2 exposes unreachable expression views" test_unreachable_expression_views;
  Test.case "ast2 exposes attribute expression and pattern views" test_attribute_views;
  Test.case "ast2 exposes extension expression pattern and item views" test_extension_views;
  Test.case "ast2 exposes locally abstract and first-class module pattern views" test_special_pattern_views;
  Test.case "ast2 exposes typed labeled parameter views" test_typed_labeled_parameter_view;
]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"syn-ast2" ~tests ~args ()) ~args:Env.args ()
