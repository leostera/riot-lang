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
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"syn-cst" ~tests ~args)
    ~args:Env.args ()
