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
            | Syn.Cst.Expression.InfixExpression expr ->
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
            | Syn.Cst.Expression.InfixExpression expr ->
                Test.assert_equal ~expected:"%>"
                  ~actual:(Syn.Cst.InfixExpression.operator expr);
                Ok ()
            | _ -> Error "expected infix expression value")
        | _ -> Error "expected first item to be a let binding");
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
              | Syn.Cst.Expression.ParenthesizedExpression expr ->
                  1 + depth (Syn.Cst.ParenthesizedExpression.inner expr)
              | _ -> 0
            in
            Test.assert_equal ~expected:5
              ~actual:(depth (Syn.Cst.LetBinding.value binding));
            Ok ()
        | _ -> Error "expected first item to be a let binding");
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"syn-cst" ~tests ~args)
    ~args:Env.args ()
