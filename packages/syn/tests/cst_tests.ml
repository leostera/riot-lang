open Std
open Syn

let expect_some value ~msg =
  match value with
  | Some value -> Ok value
  | None -> Error msg

let tests =
  [
    Test.case "cst exists for diagnostics-free parse" (fun () ->
        let result = Syn.parse ~filename:"sample.ml" "type userProfile = int\n" in
        Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
        Test.assert_true (Option.is_some result.cst);
        Ok ());
    Test.case "cst is absent when parse diagnostics exist" (fun () ->
        let result = Syn.parse ~filename:"sample.ml" "let x =\n" in
        Test.assert_true (List.length result.diagnostics > 0);
        Test.assert_true (Option.is_none result.cst);
        Ok ());
    Test.case "cst type declarations keep last module-path segment as name" (fun () ->
        let result = Syn.parse ~filename:"sample.ml" "type Message.t += Added\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = Syn.Cst.SourceFile.items cst in
        match items with
        | Syn.Cst.Item.TypeDeclaration decl :: _ ->
            Test.assert_equal ~expected:"t"
              ~actual:(Syn.Cst.Token.text (Syn.Cst.TypeDeclaration.name_token decl));
            Ok ()
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst type declarations expose direct type parameters" (fun () ->
        let result =
          Syn.parse ~filename:"sample.ml" "type ('a, 'error) resultish = int\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = Syn.Cst.SourceFile.items cst in
        match items with
        | Syn.Cst.Item.TypeDeclaration decl :: _ ->
            let params =
              Syn.Cst.TypeDeclaration.type_params decl
              |> List.filter_map Syn.Cst.TypeParameter.type_variable
              |> List.map Syn.Cst.TypeVariable.text
            in
            Test.assert_equal ~expected:[ "'a"; "'error" ] ~actual:params;
            Ok ()
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst type declarations expose record fields structurally" (fun () ->
        let result =
          Syn.parse ~filename:"sample.ml"
            "type user = { mutable userName : string; created_at : int }\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = Syn.Cst.SourceFile.items cst in
        match items with
        | Syn.Cst.Item.TypeDeclaration decl :: _ -> (
            match Syn.Cst.TypeDeclaration.type_definition decl with
            | Syn.Cst.TypeDefinition.Record fields ->
                let names =
                  fields |> List.map Syn.Cst.RecordField.name
                in
                let mutability =
                  fields |> List.map Syn.Cst.RecordField.is_mutable
                in
                Test.assert_equal ~expected:[ "userName"; "created_at" ]
                  ~actual:names;
                Test.assert_equal ~expected:[ true; false ] ~actual:mutability;
                Ok ()
            | _ -> Error "expected record type definition")
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst type declarations expose variant constructors structurally" (fun () ->
        let result =
          Syn.parse ~filename:"sample.ml"
            "type user = | Guest_user | RegisteredUser of int\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = Syn.Cst.SourceFile.items cst in
        match items with
        | Syn.Cst.Item.TypeDeclaration decl :: _ -> (
            match Syn.Cst.TypeDeclaration.type_definition decl with
            | Syn.Cst.TypeDefinition.Variant constructors ->
                let names =
                  constructors
                  |> List.map Syn.Cst.VariantConstructor.name
                  |> List.sort String.compare
                in
                Test.assert_equal ~expected:[ "Guest_user"; "RegisteredUser" ]
                  ~actual:names;
                Ok ()
            | _ -> Error "expected variant type definition")
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst type declarations expose polyvariant tags structurally" (fun () ->
        let result =
          Syn.parse ~filename:"sample.ml"
            "type user = [ `guest_user | `RegisteredUser of int ]\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = Syn.Cst.SourceFile.items cst in
        match items with
        | Syn.Cst.Item.TypeDeclaration decl :: _ -> (
            match Syn.Cst.TypeDeclaration.type_definition decl with
            | Syn.Cst.TypeDefinition.PolyVariant tags ->
                let names =
                  tags |> List.map Syn.Cst.PolyVariantTag.name
                in
                Test.assert_equal ~expected:[ "guest_user"; "RegisteredUser" ]
                  ~actual:names;
                Ok ()
            | _ -> Error "expected polyvariant type definition")
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst let bindings expose function binding names" (fun () ->
        let result = Syn.parse ~filename:"sample.ml" "let userProfile x = x\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = Syn.Cst.SourceFile.items cst in
        match items with
        | Syn.Cst.Item.LetBinding binding :: _ ->
            Test.assert_equal ~expected:"userProfile"
              ~actual:(Syn.Cst.LetBinding.name binding);
            Test.assert_true (Syn.Cst.LetBinding.is_function binding);
            Ok ()
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst let bindings distinguish value bindings from function bindings" (fun () ->
        let result = Syn.parse ~filename:"sample.ml" "let userProfile = 42\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = Syn.Cst.SourceFile.items cst in
        match items with
        | Syn.Cst.Item.LetBinding binding :: _ ->
            Test.assert_false (Syn.Cst.LetBinding.is_function binding);
            Ok ()
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst module declarations expose declared module names" (fun () ->
        let result = Syn.parse ~filename:"sample.ml" "module Foo_bar = struct end\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = Syn.Cst.SourceFile.items cst in
        match items with
        | Syn.Cst.Item.ModuleDeclaration decl :: _ ->
            Test.assert_equal ~expected:"Foo_bar"
              ~actual:(Syn.Cst.ModuleDeclaration.name decl);
            Ok ()
        | _ -> Error "expected first item to be a module declaration");
    Test.case "cst module type declarations expose declared names" (fun () ->
        let result = Syn.parse ~filename:"sample.ml" "module type Foo_bar = sig end\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = Syn.Cst.SourceFile.items cst in
        match items with
        | Syn.Cst.Item.ModuleTypeDeclaration decl :: _ ->
            Test.assert_equal ~expected:"Foo_bar"
              ~actual:(Syn.Cst.ModuleTypeDeclaration.name decl);
            Ok ()
        | _ -> Error "expected first item to be a module type declaration");
    Test.case "cst open statements preserve open! structurally" (fun () ->
        let result = Syn.parse ~filename:"sample.ml" "open! Std.List\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.OpenStatement stmt :: _ ->
            Test.assert_true (Syn.Cst.OpenStatement.has_bang stmt);
            Test.assert_equal ~expected:(Some "List")
              ~actual:
                (Syn.Cst.OpenStatement.module_path stmt
                |> Syn.Cst.ModulePath.name);
            Ok ()
        | _ -> Error "expected first item to be an open statement");
    Test.case "cst source files collect let bindings recursively" (fun () ->
        let source =
          "let top_level = 1\nlet render x = let local_value = x in local_value\n"
        in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let bindings =
          Syn.Cst.SourceFile.let_bindings cst
          |> List.map Syn.Cst.LetBinding.name
          |> List.sort String.compare
        in
        Test.assert_equal ~expected:[ "local_value"; "render"; "top_level" ]
          ~actual:bindings;
        Ok ());
    Test.case "cst let bindings expose typed parameters" (fun () ->
        let source =
          "let render userId ~displayName ?pageSize current_user = current_user\n"
        in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match
          Syn.Cst.SourceFile.let_bindings cst
          |> List.find_opt (fun binding ->
                 String.equal (Syn.Cst.LetBinding.name binding) "render")
        with
        | Some binding ->
            let names =
              Syn.Cst.LetBinding.parameters binding
              |> List.map Syn.Cst.Parameter.name
            in
            Test.assert_equal
              ~expected:
                [ Some "userId"; Some "displayName"; Some "pageSize"; Some "current_user" ]
              ~actual:names;
            Ok ()
        | None -> Error "expected render binding parameters");
    Test.case "cst let bindings preserve recursive markers" (fun () ->
        let source = "let rec loop x = loop x\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.let_bindings cst with
        | binding :: _ ->
            Test.assert_true (Syn.Cst.LetBinding.is_recursive binding);
            Ok ()
        | [] -> Error "expected recursive let binding");
    Test.case "cst let bindings expose infix string concatenation values" (fun () ->
        let source = "let banner = \"a\" ^ \"b\" ^ \"c\"\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding binding :: _ -> (
            match Syn.Cst.LetBinding.value binding with
            | Syn.Cst.Expression.Infix expr ->
                Test.assert_equal ~expected:"^"
                  ~actual:(Syn.Cst.InfixExpression.operator expr);
                Ok ()
            | _ -> Error "expected infix expression value")
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst let bindings expose custom infix operators structurally" (fun () ->
        let source = "let composed = f %> g\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding binding :: _ -> (
            match Syn.Cst.LetBinding.value binding with
            | Syn.Cst.Expression.Infix expr ->
                Test.assert_equal ~expected:"%>"
                  ~actual:(Syn.Cst.InfixExpression.operator expr);
                Ok ()
            | _ -> Error "expected infix expression value")
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst let bindings expose if expressions and unit else branches" (fun () ->
        let source = "let render ok = if ok then log () else ()\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding binding :: _ -> (
            match Syn.Cst.LetBinding.value binding with
            | Syn.Cst.Expression.If expr -> (
                match Syn.Cst.IfExpression.else_branch expr with
                | Some (Syn.Cst.Expression.Literal (Syn.Cst.Literal.Unit _)) -> Ok ()
                | _ -> Error "expected unit else branch")
            | _ -> Error "expected if expression value")
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst if expressions preserve boolean literal comparisons" (fun () ->
        let source = "let render ok = if ok = true then log () else ()\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.If
                  {
                    condition =
                      Syn.Cst.Expression.Infix
                        {
                          right =
                            Syn.Cst.Expression.Literal
                              (Syn.Cst.Literal.Bool
                                 { literal_token = { syntax_token }; _ });
                          _;
                        };
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_true
              (String.equal (Syn.Ceibo.Red.SyntaxToken.text syntax_token) "true");
            Ok ()
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst typed expressions preserve the wrapped expression and type node"
      (fun () ->
        let source = "let render = (value : user_t)\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Typed
                  {
                    expression = Syn.Cst.Expression.Path { path; _ };
                    type_syntax_node;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "value")
              ~actual:(Syn.Cst.ModulePath.name path);
            Test.assert_equal ~expected:"TYPE_CONSTR"
              ~actual:
                (type_syntax_node
                |> Ceibo.Red.SyntaxNode.kind |> SyntaxKind.to_string);
            Ok ()
        | _ -> Error "expected typed expression value");
    Test.case "cst coerce expressions preserve optional source types" (fun () ->
        let source = "let render = (value : user_t :> display_t)\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Coerce
                  {
                    expression = Syn.Cst.Expression.Path { path; _ };
                    from_type_syntax_node = Some from_type_syntax_node;
                    to_type_syntax_node;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "value")
              ~actual:(Syn.Cst.ModulePath.name path);
            Test.assert_equal ~expected:"TYPE_CONSTR"
              ~actual:
                (from_type_syntax_node
                |> Ceibo.Red.SyntaxNode.kind |> SyntaxKind.to_string);
            Test.assert_equal ~expected:"TYPE_CONSTR"
              ~actual:
                (to_type_syntax_node
                |> Ceibo.Red.SyntaxNode.kind |> SyntaxKind.to_string);
            Ok ()
        | _ -> Error "expected coerce expression value");
    Test.case "cst source files keep top-level let-in expressions as items" (fun () ->
        let source = "let a, b = pair in a\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.Expression
            (Syn.Cst.Expression.Let
              {
                binding_pattern =
                  Syn.Cst.Pattern.Tuple
                    {
                      elements =
                        [
                          Syn.Cst.Pattern.Identifier { name_token = left_name; _ };
                          Syn.Cst.Pattern.Identifier { name_token = right_name; _ };
                        ];
                      _;
                    };
                _;
              })
          :: _ ->
            Test.assert_equal ~expected:"a"
              ~actual:(Syn.Cst.Token.text left_name);
            Test.assert_equal ~expected:"b"
              ~actual:(Syn.Cst.Token.text right_name);
            Ok ()
        | _ -> Error "expected top-level let expression item");
    Test.case "cst let expressions expose unit-pattern sequencing structurally" (fun () ->
        let source = "let render () = let () = log () in flush ()\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Let
                  {
                    binding_pattern =
                      Syn.Cst.Pattern.Literal (Syn.Cst.PatternLiteral.Unit _);
                    body = Syn.Cst.Expression.Apply _;
                    _;
                  };
              _;
            }
          :: _ ->
            Ok ()
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst let bindings expose fun expressions structurally" (fun () ->
        let source = "let render = fun value -> value\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Fun
                  {
                    parameters = [ param ];
                    body = Syn.Cst.Expression.Path { path; _ };
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "value")
              ~actual:(Syn.Cst.Parameter.name param);
            Test.assert_equal ~expected:(Some "value")
              ~actual:(Syn.Cst.ModulePath.name path);
            Ok ()
        | _ -> Error "expected first item to be a let binding with a fun expression");
    Test.case "cst let bindings expose function expressions structurally" (fun () ->
        let source = "let render = function | 0 -> \"zero\" | _ -> \"other\"\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value = Syn.Cst.Expression.Function { cases; _ };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:2 ~actual:(List.length cases);
            Ok ()
        | _ -> Error "expected first item to be a let binding with a function expression");
    Test.case "cst match expressions expose boolean cases structurally" (fun () ->
        let source = "let render flag = match flag with true -> 1 | false -> 0\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    cases =
                      [
                        {
                          pattern =
                            Syn.Cst.Pattern.Literal
                              (Syn.Cst.PatternLiteral.Bool
                                 { literal_token = first; _ });
                          body = _;
                          _;
                        };
                        {
                          pattern =
                            Syn.Cst.Pattern.Literal
                              (Syn.Cst.PatternLiteral.Bool
                                 { literal_token = second; _ });
                          body = _;
                          _;
                        };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"true" ~actual:(Syn.Cst.Token.text first);
            Test.assert_equal ~expected:"false" ~actual:(Syn.Cst.Token.text second);
            Ok ()
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst try expressions expose handlers structurally" (fun () ->
        let source = "let render value = try render_inner value with exn -> raise exn\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Try
                  {
                    cases =
                      [
                        {
                          pattern = Syn.Cst.Pattern.Identifier { name_token; _ };
                          body = Syn.Cst.Expression.Apply _;
                          _;
                        };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"exn"
              ~actual:(Syn.Cst.Token.text name_token);
            Ok ()
        | _ -> Error "expected first item to be a let binding with a try expression");
    Test.case "cst source files collect recognized expressions recursively" (fun () ->
        let source = "let changed = (left <> right)\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let operators =
          Syn.Cst.SourceFile.expressions cst
          |> List.filter_map (function
               | Syn.Cst.Expression.Infix expr ->
                   Some (Syn.Cst.InfixExpression.operator expr)
               | _ -> None)
        in
        Test.assert_equal ~expected:[ "<>" ] ~actual:operators;
        Ok ());
    Test.case "cst let bindings expose apply and field access expressions structurally" (fun () ->
        let source = "let reversed = List.rev (List.rev xs)\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding binding :: _ -> (
            match Syn.Cst.LetBinding.value binding with
            | Syn.Cst.Expression.Apply outer -> (
                match Syn.Cst.ApplyExpression.callee outer with
                | Syn.Cst.Expression.FieldAccess
                    {
                      receiver = Syn.Cst.Expression.Path { path; _ };
                      field_name;
                      _;
                    } ->
                    Test.assert_equal ~expected:(Some "rev")
                      ~actual:(Some (Syn.Cst.Token.text field_name));
                    Test.assert_equal ~expected:(Some "List")
                      ~actual:(Syn.Cst.ModulePath.name path);
                    Ok ()
                | _ -> Error "expected field access callee")
            | _ -> Error "expected apply expression value")
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst field access preserves nested qualified field access structurally" (fun () ->
        let source = "let render record = record.Module.field\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.FieldAccess
                  {
                    receiver =
                      Syn.Cst.Expression.FieldAccess
                        {
                          receiver = Syn.Cst.Expression.Path { path; _ };
                          field_name = module_name;
                          _;
                        };
                    field_name;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "record")
              ~actual:(Syn.Cst.ModulePath.name path);
            Test.assert_equal ~expected:"Module"
              ~actual:(Syn.Cst.Token.text module_name);
            Test.assert_equal ~expected:"field"
              ~actual:(Syn.Cst.Token.text field_name);
            Ok ()
        | _ -> Error "expected nested field access structure");
    Test.case "cst preserves parenthesized expressions structurally" (fun () ->
        let source = "let wrapped = (((((value)))))\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding binding :: _ ->
            let rec depth = function
              | Syn.Cst.Expression.Parenthesized expr ->
                  1 + depth (Syn.Cst.ParenthesizedExpression.inner expr)
              | _ -> 0
            in
            Test.assert_equal ~expected:5
              ~actual:(depth (Syn.Cst.LetBinding.value binding));
            Ok ()
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst function cases preserve constructor tuple patterns structurally"
      (fun () ->
        let source =
          "let render = function | Some (head, _) -> head | None -> 0\n"
        in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Function
                  {
                    cases =
                      {
                        pattern =
                          Syn.Cst.Pattern.Constructor
                            {
                              constructor_path = some_path;
                              arguments =
                                [
                                  Syn.Cst.Pattern.Parenthesized
                                    {
                                      inner =
                                        Syn.Cst.Pattern.Tuple
                                          {
                                            elements =
                                              [
                                                Syn.Cst.Pattern.Identifier
                                                  { name_token = head_name; _ };
                                                Syn.Cst.Pattern.Wildcard _;
                                              ];
                                            _;
                                          };
                                      _;
                                    };
                                ];
                              _;
                            };
                        _;
                      }
                      :: {
                           pattern =
                             Syn.Cst.Pattern.Constructor
                               { constructor_path = none_path; arguments = []; _ };
                           _;
                         }
                      :: _;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "Some")
              ~actual:(Syn.Cst.ModulePath.name some_path);
            Test.assert_equal ~expected:"head"
              ~actual:(Syn.Cst.Token.text head_name);
            Test.assert_equal ~expected:(Some "None")
              ~actual:(Syn.Cst.ModulePath.name none_path);
            Ok ()
        | _ -> Error "expected faithful constructor pattern structure");
    Test.case "cst match cases preserve alias and typed patterns structurally"
      (fun () ->
        let source =
          "let render value = match value with | (user : user_t) as current_user -> current_user\n"
        in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    cases =
                      {
                        pattern =
                          Syn.Cst.Pattern.Alias
                            {
                              pattern =
                                Syn.Cst.Pattern.Typed
                                  {
                                    pattern =
                                      Syn.Cst.Pattern.Identifier
                                        { name_token = user_name; _ };
                                    type_syntax_node;
                                    _;
                                  };
                              name_token = alias_name;
                              _;
                            };
                        _;
                      }
                      :: _;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"user"
              ~actual:(Syn.Cst.Token.text user_name);
            Test.assert_equal ~expected:"current_user"
              ~actual:(Syn.Cst.Token.text alias_name);
            Test.assert_equal ~expected:"TYPE_CONSTR"
              ~actual:
                (type_syntax_node
                |> Ceibo.Red.SyntaxNode.kind |> SyntaxKind.to_string);
            Ok ()
        | _ -> Error "expected faithful alias typed pattern structure");
    Test.case "cst record expressions preserve literal fields structurally" (fun () ->
        let source = "let point = { x = 1; y = 2 }\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Record
                  (Syn.Cst.RecordExpression.Literal
                    {
                      fields =
                        [
                          { field_path = first; value = Some (Syn.Cst.Expression.Literal _); _ };
                          { field_path = second; value = Some (Syn.Cst.Expression.Literal _); _ };
                        ];
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "x")
              ~actual:(Syn.Cst.ModulePath.name first);
            Test.assert_equal ~expected:(Some "y")
              ~actual:(Syn.Cst.ModulePath.name second);
            Ok ()
        | _ -> Error "expected literal record expression");
    Test.case "cst record update expressions preserve base and updated fields"
      (fun () ->
        let source = "let point = { point with x = 3 }\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Record
                  (Syn.Cst.RecordExpression.Update
                    {
                      base = Syn.Cst.Expression.Path { path = base_path; _ };
                      fields = [ { field_path; value = Some (Syn.Cst.Expression.Literal _); _ } ];
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "point")
              ~actual:(Syn.Cst.ModulePath.name base_path);
            Test.assert_equal ~expected:(Some "x")
              ~actual:(Syn.Cst.ModulePath.name field_path);
            Ok ()
        | _ -> Error "expected update record expression");
    Test.case "cst index and assign expressions preserve the written target"
      (fun () ->
        let source = "let x = arr.(0) <- 5\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Assign
                  {
                    target =
                      Syn.Cst.Expression.Index
                        {
                          collection = Syn.Cst.Expression.Path { path; _ };
                          index =
                            Syn.Cst.Expression.Literal
                              (Syn.Cst.Literal.Int { literal_token; _ });
                          _;
                        };
                    operator_token;
                    value = Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int _);
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "arr")
              ~actual:(Syn.Cst.ModulePath.name path);
            Test.assert_equal ~expected:"0"
              ~actual:(Syn.Cst.Token.text literal_token);
            Test.assert_equal ~expected:"<-"
              ~actual:(Syn.Cst.Token.text operator_token);
            Ok ()
        | _ -> Error "expected assign(index(...)) expression");
    Test.case "cst record patterns preserve field punning and nested patterns"
      (fun () ->
        let source = "let x = match r with { user = { id }; name } -> id\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    cases =
                      [
                        {
                          pattern =
                            Syn.Cst.Pattern.Record
                              {
                                fields =
                                  [
                                    {
                                      field_path = user_field;
                                      pattern =
                                        Some
                                          (Syn.Cst.Pattern.Record
                                            {
                                              fields =
                                                [
                                                  {
                                                    field_path = id_field;
                                                    pattern = None;
                                                    _;
                                                  };
                                                ];
                                              _;
                                            });
                                      _;
                                    };
                                    { field_path = name_field; pattern = None; _ };
                                  ];
                                _;
                              };
                          _;
                        };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "user")
              ~actual:(Syn.Cst.ModulePath.name user_field);
            Test.assert_equal ~expected:(Some "id")
              ~actual:(Syn.Cst.ModulePath.name id_field);
            Test.assert_equal ~expected:(Some "name")
              ~actual:(Syn.Cst.ModulePath.name name_field);
            Ok ()
        | _ -> Error "expected record pattern structure");
    Test.case "cst array patterns preserve literal element patterns" (fun () ->
        let source = "let x = match xs with [| 1; value |] -> value\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    cases =
                      [
                        {
                          pattern =
                            Syn.Cst.Pattern.Array
                              {
                                elements =
                                  [
                                    Syn.Cst.Pattern.Literal
                                      (Syn.Cst.PatternLiteral.Int _);
                                    Syn.Cst.Pattern.Identifier { name_token; _ };
                                  ];
                                _;
                              };
                          _;
                        };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"value"
              ~actual:(Syn.Cst.Token.text name_token);
            Ok ()
        | _ -> Error "expected array pattern structure");
    Test.case "cst string indexing reuses the shared index expression shape"
      (fun () ->
        let source = "let x = s.[0]\n" in
        let result = Syn.parse ~filename:"sample.ml" source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match Syn.Cst.SourceFile.items cst with
        | Syn.Cst.Item.LetBinding
            {
              value =
                Syn.Cst.Expression.Index
                  {
                    collection = Syn.Cst.Expression.Path { path; _ };
                    index =
                      Syn.Cst.Expression.Literal
                        (Syn.Cst.Literal.Int { literal_token; _ });
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "s")
              ~actual:(Syn.Cst.ModulePath.name path);
            Test.assert_equal ~expected:"0"
              ~actual:(Syn.Cst.Token.text literal_token);
            Ok ()
        | _ -> Error "expected string index expression");
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"syn-cst" ~tests ~args)
    ~args:Env.args ()
