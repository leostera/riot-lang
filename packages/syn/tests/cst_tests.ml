open Std
open Syn

let expect_some value ~msg =
  match value with
  | Some value -> Ok value
  | None -> Error msg

let sample_ml = Path.v "sample.ml"
let sample_mli = Path.v "sample.mli"

let parse_ml source = Syn.parse ~filename:sample_ml source
let parse_mli source = Syn.parse ~filename:sample_mli source

let structure_items = function
  | Syn.Cst.Implementation { items; _ } -> items
  | Syn.Cst.Interface _ -> []

let signature_items = function
  | Syn.Cst.Interface { items; _ } -> items
  | Syn.Cst.Implementation _ -> []

let top_level_let_bindings cst =
  structure_items cst
  |> List.filter_map (function
       | Syn.Cst.StructureItem.LetBinding binding ->
           Some binding
       | _ ->
           None)

let tests =
  [
    Test.case "cst exists for diagnostics-free parse" (fun () ->
        let result = parse_ml "type userProfile = int\n" in
        Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
        Test.assert_true (Option.is_some result.cst);
        (match result.cst with
        | Some cst ->
            Test.assert_equal ~expected:`Implementation
              ~actual:(Syn.Cst.SourceFile.kind cst)
        | None -> ());
        Ok ());
    Test.case "cst root distinguishes interfaces from implementations" (fun () ->
        let result = parse_mli "val create : int -> int\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        Test.assert_equal ~expected:`Interface
          ~actual:(Syn.Cst.SourceFile.kind cst);
        Ok ());
    Test.case "cst is absent when parse diagnostics exist" (fun () ->
        let result = parse_ml "let x =\n" in
        Test.assert_true (List.length result.diagnostics > 0);
        Test.assert_true (Option.is_none result.cst);
        Ok ());
    Test.case "cst type extensions keep last module-path segment as name" (fun () ->
        let result = parse_ml "type Message.t += Added\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.TypeExtension decl :: _ ->
            Test.assert_equal ~expected:"t"
              ~actual:(Syn.Cst.Token.text (Syn.Cst.TypeExtension.name_token decl));
            Ok ()
        | _ -> Error "expected first item to be a type extension");
    Test.case "cst type extensions are preserved in interfaces" (fun () ->
        let result = parse_mli "type Message.t += Added\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.TypeExtension decl :: _ ->
            let constructors =
              Syn.Cst.TypeExtension.constructors decl
              |> List.map Syn.Cst.VariantConstructor.name
            in
            Test.assert_equal ~expected:[ "Added" ] ~actual:constructors;
            Ok ()
        | _ -> Error "expected first item to be a type extension");
    Test.case "cst interface type declarations preserve abstract and manifest forms"
      (fun () ->
        let result =
          parse_mli
            "type t\n\
             type alias = int\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.TypeDeclaration
            { type_definition = Syn.Cst.TypeDefinition.Abstract; _ }
          :: Syn.Cst.SignatureItem.TypeDeclaration
               {
                 type_definition =
                   Syn.Cst.TypeDefinition.Alias
                     {
                       manifest =
                         Syn.Cst.CoreType.Constr { constructor_path; _ };
                       _;
                     };
                 is_destructive_substitution = false;
                 _;
               }
             :: _ ->
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name constructor_path);
            Ok ()
        | _ -> Error "expected interface type declarations");
    Test.case "cst interface type declarations distinguish destructive substitutions"
      (fun () ->
        let result = parse_mli "type view := string\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.TypeDeclaration
            {
              type_name;
              type_definition =
                Syn.Cst.TypeDefinition.Alias
                  {
                    manifest =
                      Syn.Cst.CoreType.Constr { constructor_path; _ };
                    _;
                  };
              is_destructive_substitution = true;
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "view")
              ~actual:(Syn.Cst.Ident.name type_name);
            Test.assert_equal ~expected:(Some "string")
              ~actual:(Syn.Cst.Ident.name constructor_path);
            Ok ()
        | _ -> Error "expected destructive type substitution");
    Test.case "cst type declarations preserve private flags structurally"
      (fun () ->
        let result =
          parse_ml
            "type visible = int\n\
             type hidden_record = private { value : int }\n\
             type hidden_abstract = private\n\
             type hidden_variant = private Left | Right\n\
             type hidden_alias = private int\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let declarations =
          structure_items cst
          |> List.filter_map (function
               | Syn.Cst.StructureItem.TypeDeclaration decl -> Some decl
               | _ -> None)
        in
        match declarations with
        | visible :: hidden_record :: hidden_abstract :: hidden_variant :: hidden_alias :: _ ->
            Test.assert_false (Syn.Cst.TypeDeclaration.is_private visible);
            Test.assert_true (Syn.Cst.TypeDeclaration.is_private hidden_record);
            Test.assert_true (Syn.Cst.TypeDeclaration.is_private hidden_abstract);
            Test.assert_true (Syn.Cst.TypeDeclaration.is_private hidden_variant);
            Test.assert_true (Syn.Cst.TypeDeclaration.is_private hidden_alias);
            Test.assert_equal ~expected:(Some "private")
              ~actual:
                (Syn.Cst.TypeDeclaration.private_flag hidden_record
                |> Syn.Cst.PrivateFlag.private_token
                |> Option.map Syn.Cst.Token.text);
            Ok ()
        | _ -> Error "expected private and public type declarations");
    Test.case "cst type declarations expose direct type parameters" (fun () ->
        let result =
          parse_ml "type ('a, 'error) resultish = int\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.TypeDeclaration decl :: _ ->
            let params =
              Syn.Cst.TypeDeclaration.type_params decl
              |> List.filter_map Syn.Cst.TypeParameter.type_variable
              |> List.map Syn.Cst.TypeVariable.text
            in
            Test.assert_equal ~expected:[ "'a"; "'error" ] ~actual:params;
            Ok ()
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst type declarations expose parameter variance and injectivity"
      (fun () ->
        let result =
          parse_ml "type (+!'a, -'b, !'c, 'd) descriptor = int\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.TypeDeclaration decl :: _ ->
            let params = Syn.Cst.TypeDeclaration.type_params decl in
            let variances =
              params
              |> List.map (fun param ->
                     match Syn.Cst.TypeParameter.variance param with
                     | Some (Syn.Cst.TypeParameterVariance.Covariant _) ->
                         Some "covariant"
                     | Some (Syn.Cst.TypeParameterVariance.Contravariant _) ->
                         Some "contravariant"
                     | None -> None)
            in
            let injectivity =
              params |> List.map Syn.Cst.TypeParameter.is_injective
            in
            let names =
              params
              |> List.map (fun param ->
                     Syn.Cst.TypeParameter.type_variable param
                     |> Option.map Syn.Cst.TypeVariable.text)
            in
            Test.assert_equal
              ~expected:
                [ Some "covariant"; Some "contravariant"; None; None ]
              ~actual:variances;
            Test.assert_equal ~expected:[ true; false; true; false ]
              ~actual:injectivity;
            Test.assert_equal
              ~expected:
                [ Some "'a"; Some "'b"; Some "'c"; Some "'d" ]
              ~actual:names;
            Ok ()
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst type declarations expose declaration constraints" (fun () ->
        let result =
          parse_ml
            "type ('a, 'b) pair = 'a * 'b constraint 'a = int constraint 'b = string\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.TypeDeclaration decl :: _ ->
            let constraints = Syn.Cst.TypeDeclaration.constraints decl in
            let sides =
              constraints
              |> List.map (fun ({ left; right; _ } : Syn.Cst.TypeConstraint.t) ->
                     let left_name =
                       match left with
                       | Syn.Cst.CoreType.Var { name_token; _ } ->
                           Syn.Cst.Token.text name_token
                       | _ -> "<unexpected-left>"
                     in
                     let right_name =
                       match right with
                       | Syn.Cst.CoreType.Constr { constructor_path; _ } ->
                           (match Syn.Cst.Ident.name constructor_path with
                           | Some name -> name
                           | None -> "<missing-right>")
                       | _ -> "<unexpected-right>"
                     in
                     (left_name, right_name))
            in
            Test.assert_equal
              ~expected:[ ("a", "int"); ("b", "string") ]
              ~actual:sides;
            Ok ()
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst type declarations expose record fields structurally" (fun () ->
        let result =
          parse_ml
            "type user = { mutable userName : string; created_at : int }\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
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
    Test.case "cst record fields separate field attributes from field types"
      (fun () ->
        let result =
          parse_ml
            "type user = { name : int [@deprecated]; code : (string [@boxed]) }\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
            match Syn.Cst.TypeDeclaration.type_definition decl with
            | Syn.Cst.TypeDefinition.Record [ name_field; code_field ] ->
                let attribute_name ({ name; _ } : Syn.Cst.attribute) =
                  Syn.Cst.Ident.name name
                in
                let attribute_names field =
                  Syn.Cst.RecordField.attributes field
                  |> List.filter_map attribute_name
                in
                Test.assert_equal ~expected:[ "deprecated" ]
                  ~actual:(attribute_names name_field);
                Test.assert_equal ~expected:[] ~actual:(attribute_names code_field);
                (match Syn.Cst.RecordField.field_type name_field with
                | Syn.Cst.CoreType.Constr { constructor_path; _ } ->
                    Test.assert_equal ~expected:(Some "int")
                      ~actual:(Syn.Cst.Ident.name constructor_path)
                | _ ->
                    raise
                      (Failure
                         "expected attributed record field type to unwrap to int"));
                (match Syn.Cst.RecordField.field_type code_field with
                | Syn.Cst.CoreType.Parenthesized
                    {
                      inner =
                        Syn.Cst.CoreType.Attribute
                          {
                            attribute;
                            type_ = Syn.Cst.CoreType.Constr { constructor_path; _ };
                            _;
                          };
                      _;
                    } ->
                    Test.assert_equal ~expected:(Some "boxed")
                      ~actual:(attribute_name attribute);
                    Test.assert_equal ~expected:(Some "string")
                      ~actual:(Syn.Cst.Ident.name constructor_path);
                    Ok ()
                | _ ->
                    Error
                      "expected parenthesized type attribute to remain on the field type")
            | _ -> Error "expected record type definition")
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst type declarations expose variant constructors structurally" (fun () ->
        let result =
          parse_ml
            "type user = | Guest_user | RegisteredUser of int\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
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
    Test.case "cst variant constructors preserve tuple argument lists" (fun () ->
        let result =
          parse_ml
            "type coord = Point2D of int * int | Wrapped of (int * int)\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
            match Syn.Cst.TypeDeclaration.type_definition decl with
            | Syn.Cst.TypeDefinition.Variant [ point2d; wrapped ] ->
                (match Syn.Cst.VariantConstructor.arguments point2d with
                | Some (Syn.Cst.ConstructorArguments.Tuple elements) ->
                    Test.assert_equal ~expected:2 ~actual:(List.length elements)
                | _ ->
                    raise
                      (Failure
                         "expected Point2D constructor to expose two tuple arguments"));
                (match Syn.Cst.VariantConstructor.arguments wrapped with
                | Some (Syn.Cst.ConstructorArguments.Tuple [ wrapped_type ]) -> (
                    match wrapped_type with
                    | Syn.Cst.CoreType.Parenthesized
                        {
                          inner =
                            Syn.Cst.CoreType.Tuple { elements = [ _; _ ]; _ };
                          _;
                        } ->
                        Ok ()
                    | _ ->
                        Error
                          "expected Wrapped constructor to keep the parenthesized tuple as one argument")
                | _ ->
                    Error
                      "expected Wrapped constructor to expose a single tuple argument")
            | _ -> Error "expected variant type definition")
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst variant constructors preserve inline record arguments"
      (fun () ->
        let result =
          parse_ml
            "type person = Person of { name : string; age : int } | Anonymous\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
            match Syn.Cst.TypeDeclaration.type_definition decl with
            | Syn.Cst.TypeDefinition.Variant [ person; anonymous ] ->
                (match Syn.Cst.VariantConstructor.arguments person with
                | Some (Syn.Cst.ConstructorArguments.Record fields) ->
                    Test.assert_equal ~expected:[ "name"; "age" ]
                      ~actual:(List.map Syn.Cst.RecordField.name fields)
                | _ ->
                    raise
                      (Failure
                         "expected Person constructor to expose inline record fields"));
                Test.assert_equal ~expected:None
                  ~actual:(Syn.Cst.VariantConstructor.arguments anonymous);
                Ok ()
            | _ -> Error "expected variant type definition")
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst GADT constructors expose argument and result structure"
      (fun () ->
        let result =
          parse_ml
            "type _ expr = Int : int expr | Val : 'a -> 'a expr\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
            match Syn.Cst.TypeDeclaration.type_definition decl with
            | Syn.Cst.TypeDefinition.Variant [ int_constr; val_constr ] ->
                Test.assert_equal ~expected:None
                  ~actual:(Syn.Cst.VariantConstructor.arguments int_constr);
                (match Syn.Cst.VariantConstructor.result_type int_constr with
                | Some
                    (Syn.Cst.CoreType.Constr { constructor_path; arguments = [ _ ]; _ }) ->
                    Test.assert_equal ~expected:(Some "expr")
                      ~actual:(Syn.Cst.Ident.name constructor_path)
                | _ ->
                    raise
                      (Failure
                         "expected Int GADT constructor to expose its result type"));
                (match Syn.Cst.VariantConstructor.arguments val_constr with
                | Some (Syn.Cst.ConstructorArguments.Tuple [ Syn.Cst.CoreType.Var _ ]) ->
                    ()
                | _ ->
                    raise
                      (Failure
                         "expected Val GADT constructor to expose its arrow parameter"));
                (match Syn.Cst.VariantConstructor.result_type val_constr with
                | Some
                    (Syn.Cst.CoreType.Constr
                      {
                        constructor_path;
                        arguments = [ Syn.Cst.CoreType.Var { name_token; _ } ];
                        _;
                      }) ->
                    Test.assert_equal ~expected:(Some "expr")
                      ~actual:(Syn.Cst.Ident.name constructor_path);
                    Test.assert_equal ~expected:"a"
                      ~actual:(Syn.Cst.Token.text name_token);
                    Ok ()
                | _ ->
                    Error
                      "expected Val GADT constructor to expose its result type")
            | _ -> Error "expected variant type definition")
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst type extensions expose GADT constructor result types"
      (fun () ->
        let result =
          parse_ml "type _ Effect.t += Yield : unit Effect.t\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.TypeExtension decl :: _ -> (
            match Syn.Cst.TypeExtension.constructors decl with
            | [ yield ] ->
                Test.assert_equal ~expected:None
                  ~actual:(Syn.Cst.VariantConstructor.arguments yield);
                (match Syn.Cst.VariantConstructor.result_type yield with
                | Some
                    (Syn.Cst.CoreType.Constr { constructor_path; arguments = [ _ ]; _ }) ->
                    Test.assert_equal ~expected:(Some "t")
                      ~actual:(Syn.Cst.Ident.name constructor_path);
                    Ok ()
                | _ ->
                    Error
                      "expected extension constructor to expose its result type")
            | _ -> Error "expected one type-extension constructor")
        | _ -> Error "expected type extension item");
    Test.case "cst type declarations expose polyvariant tags structurally" (fun () ->
        let result =
          parse_ml
            "type user = [ `guest_user | `RegisteredUser of int ]\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
            match Syn.Cst.TypeDeclaration.type_definition decl with
            | Syn.Cst.TypeDefinition.PolyVariant poly_variant ->
                let names =
                  Syn.Cst.PolyVariant.tags poly_variant
                  |> List.map Syn.Cst.PolyVariantTag.name
                in
                Test.assert_equal ~expected:[ "guest_user"; "RegisteredUser" ]
                  ~actual:names;
                Ok ()
            | _ -> Error "expected polyvariant type definition")
        | _ -> Error "expected first item to be a type declaration");
    Test.case "cst polyvariant rows preserve inherited fields and bounds"
      (fun () ->
        let result =
          parse_mli "val cast : [> base | `Ready ] -> unit\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ValueDeclaration
            {
              type_ =
                Syn.Cst.CoreType.Arrow
                  {
                    parameter_type = Syn.Cst.CoreType.PolyVariant poly_variant;
                    result_type = Syn.Cst.CoreType.Constr _;
                    _;
                  };
              _;
            }
          :: _ -> (
            let fields = Syn.Cst.PolyVariant.fields poly_variant in
            match Syn.Cst.PolyVariant.kind poly_variant, fields with
            | Syn.Cst.PolyVariantBound.LowerBound _, [
                Syn.Cst.RowField.Inherit
                  { type_ = Syn.Cst.CoreType.Constr { constructor_path; _ }; _ };
                Syn.Cst.RowField.Tag tag;
              ] ->
                let inherited_name =
                  match Syn.Cst.Ident.name constructor_path with
                  | Some name -> name
                  | None -> ""
                in
                Test.assert_equal ~expected:"base" ~actual:inherited_name;
                Test.assert_equal ~expected:"Ready"
                  ~actual:(Syn.Cst.PolyVariantTag.name tag);
                Ok ()
            | _ ->
                Error "expected lower-bounded polyvariant row with inherited type")
        | _ -> Error "expected first item to be a value declaration");
    Test.case "cst let bindings expose function binding names" (fun () ->
        let result = parse_ml "let userProfile x = x\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.LetBinding binding :: _ ->
            Test.assert_equal ~expected:"userProfile"
              ~actual:(Syn.Cst.LetBinding.name binding);
            Test.assert_true (Syn.Cst.LetBinding.is_function binding);
            Ok ()
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst let bindings distinguish value bindings from function bindings" (fun () ->
        let result = parse_ml "let userProfile = 42\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.LetBinding binding :: _ ->
            Test.assert_false (Syn.Cst.LetBinding.is_function binding);
            Ok ()
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst module declarations preserve structure module expressions"
      (fun () ->
        let result = parse_ml "module Foo_bar = struct let answer = 42 end\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.ModuleDeclaration
            {
              module_name;
              functor_parameters = [];
              module_type = None;
              module_expression =
                Some
                  (Syn.Cst.ModuleExpression.Structure { item_syntax_nodes = [ item_node ]; _ });
              is_recursive = false;
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"Foo_bar"
              ~actual:(Syn.Cst.Token.text module_name);
            Test.assert_equal ~expected:"LET_BINDING"
              ~actual:
                (SyntaxKind.to_string
                   (Ceibo.Red.SyntaxNode.kind item_node));
            Ok ()
        | _ ->
            Error "expected module declaration with structure module expression");
    Test.case "cst module declarations preserve constrained module expressions"
      (fun () ->
        let result =
          parse_ml "module M : S with type t = int = struct type t = int end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleDeclaration
            {
              module_type =
                Some
                  (Syn.Cst.ModuleType.With
                    {
                      base = Syn.Cst.ModuleType.Path outer_base;
                      constraints = [ outer_constraint ];
                      _;
                    });
              module_expression =
                Some
                  (Syn.Cst.ModuleExpression.Constraint
                    {
                      module_expression =
                        Syn.Cst.ModuleExpression.Structure { item_syntax_nodes = [ item_node ]; _ };
                      module_type =
                        Syn.Cst.ModuleType.With
                          {
                            base = Syn.Cst.ModuleType.Path inner_base;
                            constraints = [ inner_constraint ];
                            _;
                          };
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "S")
              ~actual:(Syn.Cst.Ident.name outer_base);
            Test.assert_equal ~expected:(Some "S")
              ~actual:(Syn.Cst.Ident.name inner_base);
            (match
               ( outer_constraint.constrained_type,
                 inner_constraint.constrained_type )
             with
            | ( Syn.Cst.CoreType.Constr
                  { constructor_path = outer_path; arguments = outer_args; _ },
                Syn.Cst.CoreType.Constr
                  { constructor_path = inner_path; arguments = inner_args; _ } )
              ->
                Test.assert_equal ~expected:(Some "t")
                  ~actual:(Syn.Cst.Ident.name outer_path);
                Test.assert_equal ~expected:0 ~actual:(List.length outer_args);
                Test.assert_equal ~expected:(Some "t")
                  ~actual:(Syn.Cst.Ident.name inner_path);
                Test.assert_equal ~expected:0 ~actual:(List.length inner_args);
                Test.assert_equal ~expected:"TYPE_DECL"
                  ~actual:
                    (SyntaxKind.to_string
                       (Ceibo.Red.SyntaxNode.kind item_node));
                Ok ()
            | _ ->
                Error "expected constrained module-type targets");
        | _ ->
            Error "expected constrained module declaration");
    Test.case "cst module declarations preserve identifier module expressions"
      (fun () ->
        let result = parse_ml "module Alias = Source\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleDeclaration
            {
              module_expression = Some (Syn.Cst.ModuleExpression.Path path);
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "Source")
              ~actual:(Syn.Cst.Ident.name path);
            Ok ()
        | _ ->
            Error "expected module declaration with identifier module expression");
    Test.case "cst module declarations preserve functor module expressions"
      (fun () ->
        let result = parse_ml "module F = functor (X : S) -> X\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleDeclaration
            {
              module_expression =
                Some
                  (Syn.Cst.ModuleExpression.Functor
                    {
                      parameters =
                        [ { name_token; module_type = Syn.Cst.ModuleType.Path param_type; _ } ];
                      body = Syn.Cst.ModuleExpression.Path body_path;
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"X"
              ~actual:(Syn.Cst.Token.text name_token);
            Test.assert_equal ~expected:(Some "S")
              ~actual:(Syn.Cst.Ident.name param_type);
            Test.assert_equal ~expected:(Some "X")
              ~actual:(Syn.Cst.Ident.name body_path);
            Ok ()
        | _ ->
            Error "expected module declaration with functor module expression");
    Test.case "cst module declarations preserve functor applications structurally"
      (fun () ->
        let result = parse_ml "module M = F(X)(Y)\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleDeclaration
            {
              module_expression =
                Some
                  (Syn.Cst.ModuleExpression.Apply
                    {
                      callee =
                        Syn.Cst.ModuleExpression.Apply
                          {
                            callee = Syn.Cst.ModuleExpression.Path functor_path;
                            argument = Syn.Cst.ModuleExpression.Path first_arg;
                            _;
                          };
                      argument = Syn.Cst.ModuleExpression.Path second_arg;
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "F")
              ~actual:(Syn.Cst.Ident.name functor_path);
            Test.assert_equal ~expected:(Some "X")
              ~actual:(Syn.Cst.Ident.name first_arg);
            Test.assert_equal ~expected:(Some "Y")
              ~actual:(Syn.Cst.Ident.name second_arg);
            Ok ()
        | _ ->
            Error "expected module declaration with functor application");
    Test.case "cst module declarations preserve unit functor applications"
      (fun () ->
        let result = parse_ml "module M = F()\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleDeclaration
            {
              module_expression =
                Some
                  (Syn.Cst.ModuleExpression.ApplyUnit
                    {
                      callee = Syn.Cst.ModuleExpression.Path functor_path;
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "F")
              ~actual:(Syn.Cst.Ident.name functor_path);
            Ok ()
        | _ ->
            Error "expected module declaration with unit functor application");
    Test.case "cst module declarations preserve extension module expressions"
      (fun () ->
        let result = parse_ml "module M = [%driver]\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleDeclaration
            {
              module_expression =
                Some (Syn.Cst.ModuleExpression.Extension extension);
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"%"
              ~actual:(Syn.Cst.Token.text extension.sigil_token);
            Test.assert_equal ~expected:(Some "driver")
              ~actual:(Syn.Cst.Ident.name extension.name);
            Ok ()
        | _ ->
            Error "expected module declaration with extension module expression");
    Test.case "cst module declarations preserve unpacked first-class modules"
      (fun () ->
        let result = parse_ml "module M = (val packed : S)\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleDeclaration
            {
              module_expression =
                Some
                  (Syn.Cst.ModuleExpression.Parenthesized
                    {
                      inner =
                        Syn.Cst.ModuleExpression.Unpack
                          {
                            expression =
                              Syn.Cst.Expression.Path { path = module_path; _ };
                            module_type =
                              Some (Syn.Cst.ModuleType.Path module_type_path);
                            _;
                          };
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "packed")
              ~actual:(Syn.Cst.Ident.name module_path);
            Test.assert_equal ~expected:(Some "S")
              ~actual:(Syn.Cst.Ident.name module_type_path);
            Ok ()
        | _ ->
            Error "expected module declaration with unpacked first-class module");
    Test.case "cst recursive module items preserve grouped bindings" (fun () ->
        let result =
          parse_ml
            "module rec A : sig val x : int end = struct let x = B.y end\nand B : sig val y : int end = struct let y = 1 end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.RecursiveModuleDeclaration decl :: _ ->
            let declarations =
              Syn.Cst.RecursiveModuleDeclaration.declarations decl
            in
            let names =
              declarations |> List.map Syn.Cst.ModuleDeclaration.name
            in
            let recursive_flags =
              declarations |> List.map Syn.Cst.ModuleDeclaration.is_recursive
            in
            Test.assert_equal ~expected:[ "A"; "B" ] ~actual:names;
            Test.assert_equal ~expected:[ true; true ]
              ~actual:recursive_flags;
            Ok ()
        | _ ->
            Error "expected first item to be a recursive module declaration");
    Test.case "cst interface module declarations preserve signature-only bindings"
      (fun () ->
        let result = parse_mli "module M : sig val x : int end\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ModuleDeclaration
            {
              module_name;
              module_type = Some (Syn.Cst.ModuleType.Signature _);
              module_expression = None;
              is_recursive = false;
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"M"
              ~actual:(Syn.Cst.Token.text module_name);
            Ok ()
        | _ -> Error "expected first item to be an interface module declaration");
    Test.case "cst interface module substitutions preserve substitution flags"
      (fun () ->
        let result = parse_mli "module Alias := Std.List\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ModuleDeclaration decl :: _ -> (
            match Syn.Cst.ModuleDeclaration.module_expression decl with
            | Some (Syn.Cst.ModuleExpression.Path path) ->
                Test.assert_equal ~expected:"Alias"
                  ~actual:(Syn.Cst.ModuleDeclaration.name decl);
                Test.assert_true
                  (Syn.Cst.ModuleDeclaration.is_destructive_substitution decl);
                Test.assert_equal ~expected:(Some "List")
                  ~actual:(Syn.Cst.Ident.name path);
                Ok ()
            | _ -> Error "expected module substitution path")
        | _ -> Error "expected first item to be a module declaration");
    Test.case "cst interface recursive modules preserve grouped signatures"
      (fun () ->
        let result =
          parse_mli
            "module rec A : sig val x : int end\nand B : sig val y : int end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.RecursiveModuleDeclaration decl :: _ ->
            let declarations =
              Syn.Cst.RecursiveModuleDeclaration.declarations decl
            in
            let names =
              declarations |> List.map Syn.Cst.ModuleDeclaration.name
            in
            let signature_only =
              declarations
              |> List.map (fun declaration ->
                     Option.is_some (Syn.Cst.ModuleDeclaration.module_type declaration)
                     && Option.is_none
                          (Syn.Cst.ModuleDeclaration.module_expression declaration))
            in
            Test.assert_equal ~expected:[ "A"; "B" ] ~actual:names;
            Test.assert_equal ~expected:[ true; true ] ~actual:signature_only;
            Ok ()
        | _ ->
            Error "expected first item to be a recursive module declaration");
    Test.case "cst module type declarations expose declared names" (fun () ->
        let result = parse_ml "module type Foo_bar = sig end\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = structure_items cst in
        match items with
        | Syn.Cst.StructureItem.ModuleTypeDeclaration decl :: _ ->
            Test.assert_equal ~expected:"Foo_bar"
              ~actual:(Syn.Cst.ModuleTypeDeclaration.name decl);
            Ok ()
        | _ -> Error "expected first item to be a module type declaration");
    Test.case "cst interface module type declarations expose declared names"
      (fun () ->
        let result = parse_mli "module type Foo_bar = sig end\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let items = signature_items cst in
        match items with
        | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl :: _ ->
            Test.assert_equal ~expected:"Foo_bar"
              ~actual:(Syn.Cst.ModuleTypeDeclaration.name decl);
            Ok ()
        | _ ->
            Error "expected first item to be an interface module type declaration");
    Test.case "cst module type declarations preserve identifier module type bodies"
      (fun () ->
        let result = parse_ml "module type Alias = Source\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleTypeDeclaration
            { module_type = Some (Syn.Cst.ModuleType.Path path); _ }
          :: _ ->
            Test.assert_equal ~expected:(Some "Source")
              ~actual:(Syn.Cst.Ident.name path);
            Ok ()
        | _ ->
            Error "expected module type declaration with identifier body");
    Test.case
      "cst interface module type declarations preserve identifier module type bodies"
      (fun () ->
        let result = parse_mli "module type Alias = Source\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ModuleTypeDeclaration
            { module_type = Some (Syn.Cst.ModuleType.Path path); _ }
          :: _ ->
            Test.assert_equal ~expected:(Some "Source")
              ~actual:(Syn.Cst.Ident.name path);
            Ok ()
        | _ ->
            Error
              "expected interface module type declaration with identifier body");
    Test.case "cst module type declarations preserve signature module type bodies"
      (fun () ->
        let result = parse_ml "module type S = sig val x : int end\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleTypeDeclaration
            { module_type = Some (Syn.Cst.ModuleType.Signature _); _ }
          :: _ ->
            Ok ()
        | _ ->
            Error "expected module type declaration with signature body");
    Test.case "cst module type declarations preserve with-constraint bodies"
      (fun () ->
        let result =
          parse_ml "module type S = Driver with type config = int\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleTypeDeclaration
            {
              module_type =
                Some
                  (Syn.Cst.ModuleType.With
                    {
                      base = Syn.Cst.ModuleType.Path base_path;
                      constraints = [ constraint_ ];
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "Driver")
              ~actual:(Syn.Cst.Ident.name base_path);
            (match constraint_.constrained_type with
            | Syn.Cst.CoreType.Constr { constructor_path; arguments; _ } ->
                Test.assert_equal ~expected:(Some "config")
                  ~actual:(Syn.Cst.Ident.name constructor_path);
                Test.assert_equal ~expected:0 ~actual:(List.length arguments);
                Ok ()
            | _ ->
                Error "expected constrained module-type target")
        | _ ->
            Error "expected module type declaration with constrained body");
    Test.case "cst module type declarations preserve module-type-of bodies"
      (fun () ->
        let result =
          parse_ml "module type S = module type of Stdlib.Array\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleTypeDeclaration
            {
              module_type =
                Some
                  (Syn.Cst.ModuleType.TypeOf
                    { module_path = Syn.Cst.ModulePath.Qualified { name_token; _ }; _ });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"Array"
              ~actual:(Syn.Cst.Token.text name_token);
            Ok ()
        | _ ->
            Error "expected module type declaration with module-type-of body");
    Test.case "cst module type declarations preserve extension module type bodies"
      (fun () ->
        let result = parse_ml "module type S = [%sig_ext]\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleTypeDeclaration
            {
              module_type = Some (Syn.Cst.ModuleType.Extension extension);
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"%"
              ~actual:(Syn.Cst.Token.text extension.sigil_token);
            Test.assert_equal ~expected:(Some "sig_ext")
              ~actual:(Syn.Cst.Ident.name extension.name);
            Ok ()
        | _ ->
            Error "expected module type declaration with extension body");
    Test.case "cst interface module type substitutions preserve substitution flags"
      (fun () ->
        let result = parse_mli "module type Alias := Source\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl :: _ -> (
            match Syn.Cst.ModuleTypeDeclaration.module_type decl with
            | Some (Syn.Cst.ModuleType.Path path) ->
                Test.assert_equal ~expected:"Alias"
                  ~actual:(Syn.Cst.ModuleTypeDeclaration.name decl);
                Test.assert_true
                  (Syn.Cst.ModuleTypeDeclaration.is_destructive_substitution decl);
                Test.assert_equal ~expected:(Some "Source")
                  ~actual:(Syn.Cst.Ident.name path);
                Ok ()
            | _ -> Error "expected module type substitution path")
        | _ ->
            Error "expected first item to be an interface module type declaration");
    Test.case "cst interface class declarations preserve typed class-type annotations"
      (fun () ->
        let result =
          parse_mli "class c : object method x : int end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ClassDeclaration
            {
              class_name;
              class_type =
                Some
                  (Syn.Cst.ClassType.Signature
                    {
                      syntax_node = class_type_syntax_node;
                      fields =
                        [
                          Syn.Cst.ClassTypeField.Method
                            {
                              name_token = class_type_method_name;
                              type_ =
                                Syn.Cst.CoreType.Constr
                                  {
                                    constructor_path = declared_type;
                                    _;
                                  };
                              _;
                            };
                        ];
                    });
              class_body = None;
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"c"
              ~actual:(Syn.Cst.Token.text class_name);
            Test.assert_equal ~expected:"OBJECT_EXPR"
              ~actual:
                (SyntaxKind.to_string
                   (Ceibo.Red.SyntaxNode.kind class_type_syntax_node));
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text class_type_method_name);
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name declared_type);
            Ok ()
        | _ -> Error "expected interface class declaration");
    Test.case
      "cst implementation class declarations preserve declaration-site class-type annotations"
      (fun () ->
        let result =
          parse_ml
            "class service : object method run : response end = object method run = value end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ClassDeclaration
            {
              class_name;
              class_type =
                Some
                  (Syn.Cst.ClassType.Signature
                    {
                      fields =
                        [
                          Syn.Cst.ClassTypeField.Method
                            {
                              name_token = method_name;
                              type_ =
                                Syn.Cst.CoreType.Constr
                                  { constructor_path = response_type; _ };
                              _;
                            };
                        ];
                      _;
                    });
              class_body =
                Some
                  (Syn.Cst.ClassExpression.Structure
                    {
                      fields =
                        [
                            Syn.Cst.ClassField.Method
                              {
                                name_token = body_method_name;
                                body =
                                  Some
                                    (Syn.Cst.Expression.Path
                                      { path = body_value_path; _ });
                                _;
                              };
                        ];
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"service"
              ~actual:(Syn.Cst.Token.text class_name);
            Test.assert_equal ~expected:"run"
              ~actual:(Syn.Cst.Token.text method_name);
            Test.assert_equal ~expected:(Some "response")
              ~actual:(Syn.Cst.Ident.name response_type);
            Test.assert_equal ~expected:"run"
              ~actual:(Syn.Cst.Token.text body_method_name);
            Test.assert_equal ~expected:(Some "value")
              ~actual:(Syn.Cst.Ident.name body_value_path);
            Ok ()
        | _ -> Error "expected implementation class declaration");
    Test.case "cst class declarations preserve typed class-expression forms"
      (fun () ->
        let result =
          parse_ml
            "class direct = builder\n\
             class applied = builder arg\n\
             class factory = fun x -> object method run = x end\n\
             class local = let helper = 1 in object method run = helper end\n\
             class opened = M.(builder)\n\
             class generated = [%class_body]\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ClassDeclaration
            {
              class_name = direct_name;
              class_body =
                Some (Syn.Cst.ClassExpression.Path direct_path);
              _;
            }
          :: Syn.Cst.StructureItem.ClassDeclaration
               {
                 class_name = applied_name;
                 class_body =
                   Some
                     (Syn.Cst.ClassExpression.Apply
                       {
                         callee = Syn.Cst.ClassExpression.Path applied_callee;
                         argument =
                           Syn.Cst.Positional
                             (Syn.Cst.Expression.Path { path = applied_arg; _ });
                         _;
                       });
                 _;
               }
             :: Syn.Cst.StructureItem.ClassDeclaration
                  {
                    class_name = factory_name;
                    class_body =
                      Some
                        (Syn.Cst.ClassExpression.Fun
                          {
                            parameters =
                              [
                                Syn.Cst.Parameter.Positional
                                  { name_token = Some factory_param; _ };
                              ];
                            body =
                              Syn.Cst.ClassExpression.Structure
                                {
                                  fields =
                                    [
                                      Syn.Cst.ClassField.Method
                                        {
                                          name_token = factory_method_name;
                                          body =
                                            Some
                                              (Syn.Cst.Expression.Path
                                                { path = factory_body_path; _ });
                                          _;
                                        };
                                    ];
                                  _;
                                };
                            _;
                          });
                    _;
                  }
                :: Syn.Cst.StructureItem.ClassDeclaration
                     {
                       class_name = local_name;
                       class_body =
                         Some
                           (Syn.Cst.ClassExpression.Let
                             {
                               binding_pattern =
                                 Syn.Cst.Pattern.Identifier
                                   { name_token = helper_name; _ };
                               bound_value =
                                 Syn.Cst.Expression.Literal
                                   (Syn.Cst.Literal.Int { literal_token; _ });
                               body =
                                 Syn.Cst.ClassExpression.Structure
                                   {
                                     fields =
                                       [
                                         Syn.Cst.ClassField.Method
                                           {
                                             body =
                                               Some
                                                 (Syn.Cst.Expression.Path
                                                   { path = helper_path; _ });
                                             _;
                                           };
                                       ];
                                     _;
                                   };
                               _;
                             });
                       _;
                     }
                   :: Syn.Cst.StructureItem.ClassDeclaration
                        {
                          class_name = opened_name;
                          class_body =
                            Some
                              (Syn.Cst.ClassExpression.LocalOpen
                                {
                                  module_path = opened_module_path;
                                  class_expression =
                                    Syn.Cst.ClassExpression.Path opened_body_path;
                                  _;
                                });
                          _;
                        }
                      :: Syn.Cst.StructureItem.ClassDeclaration
                           {
                             class_name = generated_name;
                             class_body =
                               Some
                                 (Syn.Cst.ClassExpression.Extension extension);
                             _;
                           }
                         :: _ ->
            Test.assert_equal ~expected:"direct"
              ~actual:(Syn.Cst.Token.text direct_name);
            Test.assert_equal ~expected:(Some "builder")
              ~actual:(Syn.Cst.Ident.name direct_path);
            Test.assert_equal ~expected:"applied"
              ~actual:(Syn.Cst.Token.text applied_name);
            Test.assert_equal ~expected:(Some "builder")
              ~actual:(Syn.Cst.Ident.name applied_callee);
            Test.assert_equal ~expected:(Some "arg")
              ~actual:(Syn.Cst.Ident.name applied_arg);
            Test.assert_equal ~expected:"factory"
              ~actual:(Syn.Cst.Token.text factory_name);
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text factory_param);
            Test.assert_equal ~expected:"run"
              ~actual:(Syn.Cst.Token.text factory_method_name);
            Test.assert_equal ~expected:(Some "x")
              ~actual:(Syn.Cst.Ident.name factory_body_path);
            Test.assert_equal ~expected:"local"
              ~actual:(Syn.Cst.Token.text local_name);
            Test.assert_equal ~expected:"helper"
              ~actual:(Syn.Cst.Token.text helper_name);
            Test.assert_equal ~expected:"1"
              ~actual:(Syn.Cst.Token.text literal_token);
            Test.assert_equal ~expected:(Some "helper")
              ~actual:(Syn.Cst.Ident.name helper_path);
            Test.assert_equal ~expected:"opened"
              ~actual:(Syn.Cst.Token.text opened_name);
            Test.assert_equal ~expected:(Some "M")
              ~actual:(Syn.Cst.Ident.name opened_module_path);
            Test.assert_equal ~expected:(Some "builder")
              ~actual:(Syn.Cst.Ident.name opened_body_path);
            Test.assert_equal ~expected:"generated"
              ~actual:(Syn.Cst.Token.text generated_name);
            Test.assert_equal ~expected:(Some "class_body")
              ~actual:(Syn.Cst.Ident.name extension.name);
            Ok ()
        | _ -> Error "expected typed class-expression declarations");
    Test.case "cst class declarations preserve constrained class-expression bodies"
      (fun () ->
        let result =
          parse_ml "class constrained = (builder : service)\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ClassDeclaration
            {
              class_name;
              class_body =
                Some
                  (Syn.Cst.ClassExpression.Constraint
                    {
                      class_expression =
                        Syn.Cst.ClassExpression.Path builder_path;
                      class_type = Syn.Cst.ClassType.Path service_path;
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"constrained"
              ~actual:(Syn.Cst.Token.text class_name);
            Test.assert_equal ~expected:(Some "builder")
              ~actual:(Syn.Cst.Ident.name builder_path);
            Test.assert_equal ~expected:(Some "service")
              ~actual:(Syn.Cst.Ident.name service_path);
            Ok ()
        | _ -> Error "expected constrained class-expression declaration");
    Test.case "cst class structures preserve field attributes" (fun () ->
        let result =
          parse_ml
            "class c = object\n\
             \  inherit builder [@@inh]\n\
             \  val state = seed [@@tracked]\n\
             \  method run = state [@@trace]\n\
             \  constraint t = int [@@eq]\n\
             \  initializer ignore state [@@init]\n\
             end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ClassDeclaration
            {
              class_body =
                Some
                  (Syn.Cst.ClassExpression.Structure
                    {
                      fields =
                        [
                          Syn.Cst.ClassField.Attribute
                            {
                              field =
                                Syn.Cst.ClassField.Inherit
                                  {
                                    class_expression =
                                      Syn.Cst.ClassExpression.Path inherited_class;
                                    _;
                                  };
                              attribute = inherit_attribute;
                              _;
                            };
                          Syn.Cst.ClassField.Attribute
                            {
                              field =
                                Syn.Cst.ClassField.Value
                                  {
                                    name_token = state_name;
                                    value =
                                      Some
                                        (Syn.Cst.Expression.Path
                                          { path = state_value; _ });
                                    _;
                                  };
                              attribute = state_attribute;
                              _;
                            };
                          Syn.Cst.ClassField.Attribute
                            {
                              field =
                                Syn.Cst.ClassField.Method
                                  {
                                    name_token = run_name;
                                    body =
                                      Some
                                        (Syn.Cst.Expression.Path
                                          { path = run_body; _ });
                                    _;
                                  };
                              attribute = run_attribute;
                              _;
                            };
                          Syn.Cst.ClassField.Attribute
                            {
                              field =
                                Syn.Cst.ClassField.Constraint
                                  {
                                    left =
                                      Syn.Cst.CoreType.Constr
                                        { constructor_path = left_type; _ };
                                    right =
                                      Syn.Cst.CoreType.Constr
                                        { constructor_path = right_type; _ };
                                    _;
                                  };
                              attribute = constraint_attribute;
                              _;
                            };
                          Syn.Cst.ClassField.Attribute
                            {
                              field =
                                Syn.Cst.ClassField.Initializer
                                  {
                                    body =
                                      Some
                                        (Syn.Cst.Expression.Apply
                                          {
                                            callee =
                                              Syn.Cst.Expression.Path
                                                { path = init_callee; _ };
                                            argument =
                                              Syn.Cst.Positional
                                                (Syn.Cst.Expression.Path
                                                  { path = init_arg; _ });
                                            _;
                                          });
                                    _;
                                  };
                              attribute = init_attribute;
                              _;
                            };
                        ];
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "builder")
              ~actual:(Syn.Cst.Ident.name inherited_class);
            Test.assert_equal ~expected:"@@"
              ~actual:(Syn.Cst.Token.text inherit_attribute.sigil_token);
            Test.assert_equal ~expected:"state"
              ~actual:(Syn.Cst.Token.text state_name);
            Test.assert_equal ~expected:(Some "seed")
              ~actual:(Syn.Cst.Ident.name state_value);
            Test.assert_equal ~expected:"@@"
              ~actual:(Syn.Cst.Token.text state_attribute.sigil_token);
            Test.assert_equal ~expected:"run"
              ~actual:(Syn.Cst.Token.text run_name);
            Test.assert_equal ~expected:(Some "state")
              ~actual:(Syn.Cst.Ident.name run_body);
            Test.assert_equal ~expected:"@@"
              ~actual:(Syn.Cst.Token.text run_attribute.sigil_token);
            Test.assert_equal ~expected:(Some "t")
              ~actual:(Syn.Cst.Ident.name left_type);
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name right_type);
            Test.assert_equal ~expected:"@@"
              ~actual:(Syn.Cst.Token.text constraint_attribute.sigil_token);
            Test.assert_equal ~expected:(Some "ignore")
              ~actual:(Syn.Cst.Ident.name init_callee);
            Test.assert_equal ~expected:(Some "state")
              ~actual:(Syn.Cst.Ident.name init_arg);
            Test.assert_equal ~expected:"@@"
              ~actual:(Syn.Cst.Token.text init_attribute.sigil_token);
            Ok ()
        | _ -> Error "expected class fields wrapped with attributes");
    Test.case "cst class structures preserve class fields, constraints, and extensions"
      (fun () ->
        let result =
          parse_ml
            "class c = object\n\
             \  val mutable state = seed\n\
             \  inherit (builder arg)\n\
             \  method private run = state\n\
             \  constraint t = int\n\
             \  [%%field]\n\
             \  initializer ignore state\n\
             end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ClassDeclaration
            {
              class_body =
                Some
                  (Syn.Cst.ClassExpression.Structure
                    {
                      fields =
                        [
                          Syn.Cst.ClassField.Value
                            {
                              name_token = state_name;
                              value =
                                Some
                                  (Syn.Cst.Expression.Path { path = state_value; _ });
                              is_mutable = true;
                              _;
                            };
                          Syn.Cst.ClassField.Inherit
                            {
                              class_expression =
                                Syn.Cst.ClassExpression.Parenthesized
                                  {
                                    inner =
                                      Syn.Cst.ClassExpression.Apply
                                        {
                                          callee =
                                            Syn.Cst.ClassExpression.Path
                                              inherit_callee;
                                          argument =
                                            Syn.Cst.Positional
                                              (Syn.Cst.Expression.Path
                                                { path = inherit_arg; _ });
                                          _;
                                        };
                                    _;
                                  };
                              _;
                            };
                          Syn.Cst.ClassField.Method
                            {
                              name_token = method_name;
                              body =
                                Some
                                  (Syn.Cst.Expression.Path { path = method_body; _ });
                              is_private = true;
                              _;
                            };
                          Syn.Cst.ClassField.Constraint
                            {
                              left =
                                Syn.Cst.CoreType.Constr
                                  { constructor_path = left_type; _ };
                              right =
                                Syn.Cst.CoreType.Constr
                                  { constructor_path = right_type; _ };
                              _;
                            };
                          Syn.Cst.ClassField.Extension extension;
                          Syn.Cst.ClassField.Initializer
                            {
                              body =
                                Some
                                  (Syn.Cst.Expression.Apply
                                    {
                                      callee =
                                        Syn.Cst.Expression.Path
                                          { path = init_callee; _ };
                                      argument =
                                        Syn.Cst.Positional
                                          (Syn.Cst.Expression.Path
                                            { path = init_arg; _ });
                                      _;
                                    });
                              _;
                            };
                        ];
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"state"
              ~actual:(Syn.Cst.Token.text state_name);
            Test.assert_equal ~expected:(Some "seed")
              ~actual:(Syn.Cst.Ident.name state_value);
            Test.assert_equal ~expected:(Some "builder")
              ~actual:(Syn.Cst.Ident.name inherit_callee);
            Test.assert_equal ~expected:(Some "arg")
              ~actual:(Syn.Cst.Ident.name inherit_arg);
            Test.assert_equal ~expected:"run"
              ~actual:(Syn.Cst.Token.text method_name);
            Test.assert_equal ~expected:(Some "state")
              ~actual:(Syn.Cst.Ident.name method_body);
            Test.assert_equal ~expected:(Some "t")
              ~actual:(Syn.Cst.Ident.name left_type);
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name right_type);
            Test.assert_equal ~expected:(Some "field")
              ~actual:(Syn.Cst.Ident.name extension.name);
            Test.assert_equal ~expected:(Some "ignore")
              ~actual:(Syn.Cst.Ident.name init_callee);
            Test.assert_equal ~expected:(Some "state")
              ~actual:(Syn.Cst.Ident.name init_arg);
            Ok ()
        | _ -> Error "expected structured class fields");
    Test.case "cst interface class type declarations preserve structured signatures"
      (fun () ->
        let result =
          parse_mli "class type ct = object method x : int end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ClassTypeDeclaration
            {
              class_type_name;
              class_type_body =
                Syn.Cst.ClassType.Signature
                  {
                    syntax_node = class_type_body_syntax_node;
                    fields =
                      [
                        Syn.Cst.ClassTypeField.Method
                          {
                            name_token;
                            type_ =
                              Syn.Cst.CoreType.Constr { constructor_path; _ };
                            _;
                          };
                      ];
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"ct"
              ~actual:(Syn.Cst.Token.text class_type_name);
            Test.assert_equal ~expected:"OBJECT_EXPR"
              ~actual:
                (SyntaxKind.to_string
                   (Ceibo.Red.SyntaxNode.kind class_type_body_syntax_node));
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text name_token);
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name constructor_path);
            Ok ()
        | _ -> Error "expected interface class type declaration");
    Test.case "cst class type declarations preserve path, local-open, and extension bodies"
      (fun () ->
        let result =
          parse_ml
            "class type direct = C\n\
             class type opened = M.(C)\n\
             class type generated = ([%ct])\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ClassTypeDeclaration
            { class_type_body = Syn.Cst.ClassType.Path direct_path; _ }
          :: Syn.Cst.StructureItem.ClassTypeDeclaration
               {
                 class_type_body =
                   Syn.Cst.ClassType.LocalOpen
                     {
                       module_path = open_module;
                       class_type = Syn.Cst.ClassType.Path opened_path;
                       _;
                     };
                 _;
               }
             :: Syn.Cst.StructureItem.ClassTypeDeclaration
                  {
                    class_type_body =
                      Syn.Cst.ClassType.Parenthesized
                        {
                          inner = Syn.Cst.ClassType.Extension extension;
                          _;
                        };
                    _;
                  }
                :: _ ->
            Test.assert_equal ~expected:(Some "C")
              ~actual:(Syn.Cst.Ident.name direct_path);
            Test.assert_equal ~expected:(Some "M")
              ~actual:(Syn.Cst.Ident.name open_module);
            Test.assert_equal ~expected:(Some "C")
              ~actual:(Syn.Cst.Ident.name opened_path);
            Test.assert_equal ~expected:(Some "ct")
              ~actual:(Syn.Cst.Ident.name extension.name);
            Ok ()
        | _ -> Error "expected class type path/local-open/extension bodies");
    Test.case "cst interface class declarations preserve arrow-style class types"
      (fun () ->
        let result =
          parse_mli "class service : request -> object method run : int end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ClassDeclaration
            {
              class_name;
              class_type =
                Some
                  (Syn.Cst.ClassType.Arrow
                    {
                      parameter_type =
                        Syn.Cst.CoreType.Constr
                          { constructor_path = request_type; _ };
                      result_type =
                        Syn.Cst.ClassType.Signature
                          {
                            fields =
                              [
                                Syn.Cst.ClassTypeField.Method
                                  {
                                    name_token = method_name;
                                    type_ =
                                      Syn.Cst.CoreType.Constr
                                        { constructor_path = method_type; _ };
                                    _;
                                  };
                              ];
                            _;
                          };
                      _;
                    });
              class_body = None;
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"service"
              ~actual:(Syn.Cst.Token.text class_name);
            Test.assert_equal ~expected:(Some "request")
              ~actual:(Syn.Cst.Ident.name request_type);
            Test.assert_equal ~expected:"run"
              ~actual:(Syn.Cst.Token.text method_name);
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name method_type);
            Ok ()
        | _ -> Error "expected interface class declaration with arrow class type");
    Test.case "cst class type declarations preserve arrow and parenthesized arrow bodies"
      (fun () ->
        let result =
          parse_ml
            "class type factory = request -> response -> service\n\
             class type grouped = (request -> service)\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ClassTypeDeclaration
            {
              class_type_name = factory_name;
              class_type_body =
                Syn.Cst.ClassType.Arrow
                  {
                    parameter_type =
                      Syn.Cst.CoreType.Constr
                        { constructor_path = request_type; _ };
                    result_type =
                      Syn.Cst.ClassType.Arrow
                        {
                          parameter_type =
                            Syn.Cst.CoreType.Constr
                              { constructor_path = response_type; _ };
                          result_type = Syn.Cst.ClassType.Path service_type;
                          _;
                        };
                    _;
                  };
              _;
            }
          :: Syn.Cst.StructureItem.ClassTypeDeclaration
               {
                 class_type_name = grouped_name;
                 class_type_body =
                   Syn.Cst.ClassType.Parenthesized
                     {
                       inner =
                         Syn.Cst.ClassType.Arrow
                           {
                             parameter_type =
                               Syn.Cst.CoreType.Constr
                                 { constructor_path = grouped_request; _ };
                             result_type = Syn.Cst.ClassType.Path grouped_service;
                             _;
                           };
                       _;
                     };
                 _;
               }
             :: _ ->
            Test.assert_equal ~expected:"factory"
              ~actual:(Syn.Cst.Token.text factory_name);
            Test.assert_equal ~expected:(Some "request")
              ~actual:(Syn.Cst.Ident.name request_type);
            Test.assert_equal ~expected:(Some "response")
              ~actual:(Syn.Cst.Ident.name response_type);
            Test.assert_equal ~expected:(Some "service")
              ~actual:(Syn.Cst.Ident.name service_type);
            Test.assert_equal ~expected:"grouped"
              ~actual:(Syn.Cst.Token.text grouped_name);
            Test.assert_equal ~expected:(Some "request")
              ~actual:(Syn.Cst.Ident.name grouped_request);
            Test.assert_equal ~expected:(Some "service")
              ~actual:(Syn.Cst.Ident.name grouped_service);
            Ok ()
        | _ -> Error "expected arrow-style class type declarations");
    Test.case "cst class type signatures preserve field structure and field attributes"
      (fun () ->
        let result =
          parse_ml
            "class type ct = object\n\
             \  inherit base [@@foo]\n\
             \  val mutable state : int [@@foo]\n\
             \  method private close : string [@@foo]\n\
             \  constraint t = int [@@foo]\n\
             end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ClassTypeDeclaration
            {
              class_type_body =
                Syn.Cst.ClassType.Signature
                  {
                    fields =
                      [
                        Syn.Cst.ClassTypeField.Attribute
                          {
                            field =
                              Syn.Cst.ClassTypeField.Inherit
                                {
                                  class_type =
                                    Syn.Cst.ClassType.Path inherited_class;
                                  _;
                                };
                            attribute = inherit_attribute;
                            _;
                          };
                        Syn.Cst.ClassTypeField.Attribute
                          {
                            field =
                              Syn.Cst.ClassTypeField.Value
                                {
                                  name_token = state_name;
                                  type_ =
                                    Syn.Cst.CoreType.Constr
                                      { constructor_path = state_type; _ };
                                  is_mutable = true;
                                  _;
                                };
                            attribute = state_attribute;
                            _;
                          };
                        Syn.Cst.ClassTypeField.Attribute
                          {
                            field =
                              Syn.Cst.ClassTypeField.Method
                                {
                                  name_token = close_name;
                                  type_ =
                                    Syn.Cst.CoreType.Constr
                                      { constructor_path = close_type; _ };
                                  is_private = true;
                                  _;
                                };
                            attribute = method_attribute;
                            _;
                          };
                        Syn.Cst.ClassTypeField.Attribute
                          {
                            field =
                              Syn.Cst.ClassTypeField.Constraint
                                {
                                  left =
                                    Syn.Cst.CoreType.Constr
                                      { constructor_path = left_type; _ };
                                  right =
                                    Syn.Cst.CoreType.Constr
                                      { constructor_path = right_type; _ };
                                  _;
                                };
                            attribute = constraint_attribute;
                            _;
                          };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "base")
              ~actual:(Syn.Cst.Ident.name inherited_class);
            Test.assert_equal ~expected:"@@"
              ~actual:(Syn.Cst.Token.text inherit_attribute.sigil_token);
            Test.assert_equal ~expected:"state"
              ~actual:(Syn.Cst.Token.text state_name);
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name state_type);
            Test.assert_equal ~expected:"@@"
              ~actual:(Syn.Cst.Token.text state_attribute.sigil_token);
            Test.assert_equal ~expected:"close"
              ~actual:(Syn.Cst.Token.text close_name);
            Test.assert_equal ~expected:(Some "string")
              ~actual:(Syn.Cst.Ident.name close_type);
            Test.assert_equal ~expected:"@@"
              ~actual:(Syn.Cst.Token.text method_attribute.sigil_token);
            Test.assert_equal ~expected:(Some "t")
              ~actual:(Syn.Cst.Ident.name left_type);
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name right_type);
            Test.assert_equal ~expected:"@@"
              ~actual:(Syn.Cst.Token.text constraint_attribute.sigil_token);
            Ok ()
        | _ -> Error "expected structured class type signature");
    Test.case "cst class type signatures preserve extension fields" (fun () ->
        let result =
          parse_ml
            "class type ct = object\n\
             \  [%%foo]\n\
             \  method run : int\n\
             end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ClassTypeDeclaration
            {
              class_type_body =
                Syn.Cst.ClassType.Signature
                  {
                    fields =
                      [
                        Syn.Cst.ClassTypeField.Extension extension;
                        Syn.Cst.ClassTypeField.Method
                          {
                            name_token;
                            type_ =
                              Syn.Cst.CoreType.Constr { constructor_path; _ };
                            _;
                          };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"%"
              ~actual:(Syn.Cst.Token.text extension.sigil_token);
            Test.assert_equal ~expected:(Some "foo")
              ~actual:(Syn.Cst.Ident.name extension.name);
            Test.assert_equal ~expected:"run"
              ~actual:(Syn.Cst.Token.text name_token);
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name constructor_path);
            Ok ()
        | _ -> Error "expected class type extension field");
    Test.case "cst value declarations preserve names and type nodes" (fun () ->
        let result = parse_mli "val create : name:string -> person\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ValueDeclaration
            {
              name_token;
              type_ =
                Syn.Cst.CoreType.Arrow
                  {
                    label =
                      Some
                        (Syn.Cst.ArrowLabel.Named
                          { sigil_token = None; label_token; });
                    parameter_type =
                      Syn.Cst.CoreType.Constr
                        { constructor_path = parameter_path; _ };
                    result_type =
                      Syn.Cst.CoreType.Constr
                        { constructor_path = result_path; _ };
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"create"
              ~actual:(Syn.Cst.Token.text name_token);
            Test.assert_equal ~expected:"name"
              ~actual:(Syn.Cst.Token.text label_token);
            Test.assert_equal ~expected:(Some "string")
              ~actual:(Syn.Cst.Ident.name parameter_path);
            Test.assert_equal ~expected:(Some "person")
              ~actual:(Syn.Cst.Ident.name result_path);
            Ok ()
        | _ -> Error "expected first item to be a value declaration");
    Test.case "cst value declarations preserve optional arrow labels" (fun () ->
        let result = parse_mli "val create : ?state:string -> person\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ValueDeclaration
            {
              type_ =
                Syn.Cst.CoreType.Arrow
                  {
                    label =
                      Some
                        (Syn.Cst.ArrowLabel.OptionalNamed
                          { sigil_token; label_token; });
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"?"
              ~actual:(Syn.Cst.Token.text sigil_token);
            Test.assert_equal ~expected:"state"
              ~actual:(Syn.Cst.Token.text label_token);
            Ok ()
        | _ -> Error "expected optional labeled value declaration");
    Test.case "cst value declarations lift explicit polymorphic core types"
      (fun () ->
        let result = parse_mli "val id : 'a 'b. 'a -> 'b -> 'a\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ValueDeclaration
            {
              type_ =
                Syn.Cst.CoreType.Poly
                  {
                    binders;
                    body =
                      Syn.Cst.CoreType.Arrow
                        {
                          result_type = Syn.Cst.CoreType.Arrow _;
                          _;
                        };
                    _;
                  };
              _;
            }
          :: _ ->
            let binder_text =
              binders |> List.map Syn.Cst.TypeBinder.text
            in
            let quoted =
              binders |> List.map Syn.Cst.TypeBinder.is_quoted
            in
            Test.assert_equal ~expected:[ "'a"; "'b" ] ~actual:binder_text;
            Test.assert_equal ~expected:[ true; true ] ~actual:quoted;
            Ok ()
        | _ -> Error "expected explicitly polymorphic value declaration");
    Test.case "cst value declarations preserve package core types"
      (fun () ->
        let result =
          parse_mli "val driver : (module Driver with type config = int)\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ValueDeclaration
            {
              type_ =
                Syn.Cst.CoreType.FirstClassModule
                  {
                    module_type =
                      Syn.Cst.ModuleType.With
                        {
                          base = Syn.Cst.ModuleType.Path base_path;
                          constraints;
                          _;
                        };
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "Driver")
              ~actual:(Syn.Cst.ModulePath.name base_path);
            Test.assert_equal ~expected:1 ~actual:(List.length constraints);
            Ok ()
        | _ -> Error "expected package core type");
    Test.case "cst value declarations preserve locally opened core types"
      (fun () ->
        let result =
          parse_mli "val decode : Outer.Inner.(request -> response)\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ValueDeclaration
            {
              type_ =
                Syn.Cst.CoreType.LocalOpen
                  {
                    module_path =
                      Syn.Cst.ModulePath.Qualified
                        {
                          prefix =
                            Syn.Cst.ModulePath.Ident { name_token = outer_module; _ };
                          name_token = inner_module;
                          _;
                        };
                    type_ =
                      Syn.Cst.CoreType.Arrow
                        {
                          parameter_type =
                            Syn.Cst.CoreType.Constr
                              { constructor_path = parameter_path; _ };
                          result_type =
                            Syn.Cst.CoreType.Constr
                              { constructor_path = result_path; _ };
                          _;
                        };
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"Outer"
              ~actual:(Syn.Cst.Token.text outer_module);
            Test.assert_equal ~expected:"Inner"
              ~actual:(Syn.Cst.Token.text inner_module);
            Test.assert_equal ~expected:(Some "request")
              ~actual:(Syn.Cst.Ident.name parameter_path);
            Test.assert_equal ~expected:(Some "response")
              ~actual:(Syn.Cst.Ident.name result_path);
            Ok ()
        | _ -> Error "expected local-open core type");
    Test.case "cst external declarations preserve primitive names" (fun () ->
        let result =
          parse_ml
            "external sqrt : float -> float = \"caml_sqrt_float\"\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ExternalDeclaration { name_token; primitive_name_tokens; _ } :: _ ->
            Test.assert_equal ~expected:"sqrt"
              ~actual:(Syn.Cst.Token.text name_token);
            Test.assert_equal ~expected:[ "\"caml_sqrt_float\"" ]
              ~actual:(List.map Syn.Cst.Token.text primitive_name_tokens);
            Ok ()
        | _ -> Error "expected first item to be an external declaration");
    Test.case "cst include statements preserve typed include targets" (fun () ->
        let result =
          parse_mli "include module type of Stdlib.Array\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.IncludeStatement
            { target = Syn.Cst.ModuleType (Syn.Cst.ModuleType.TypeOf { module_path; _ }); _ }
          :: _ ->
            Test.assert_equal ~expected:(Some "Array")
              ~actual:(Syn.Cst.Ident.name module_path);
            Ok ()
        | _ -> Error "expected first item to be an include statement");
    Test.case "cst implementation includes preserve module-expression targets" (fun () ->
        let result = parse_ml "include Std.List\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.IncludeStatement
            {
              target =
                Syn.Cst.ModuleExpression
                  (Syn.Cst.ModuleExpression.Path
                    (Syn.Cst.Ident.Qualified
                      {
                        prefix = Syn.Cst.Ident.Ident { name_token = root; _ };
                        name_token = leaf;
                        _;
                      }));
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"Std" ~actual:(Syn.Cst.Token.text root);
            Test.assert_equal ~expected:"List" ~actual:(Syn.Cst.Token.text leaf);
            Ok ()
        | _ -> Error "expected first item to be an include statement");
    Test.case "cst source files distinguish standalone attribute items"
      (fun () ->
        let result =
          parse_ml "[@@@attr]\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.Attribute attribute :: _ ->
            Test.assert_equal ~expected:"ATTRIBUTE_EXPR"
              ~actual:
                (SyntaxKind.to_string
                   (Ceibo.Red.SyntaxNode.kind attribute.syntax_node));
            Test.assert_equal ~expected:"@@"
              ~actual:(Syn.Cst.Token.text attribute.sigil_token);
            Test.assert_equal ~expected:(Some "attr")
              ~actual:(Syn.Cst.Ident.name attribute.name);
            Test.assert_equal ~expected:None ~actual:attribute.payload_syntax_node;
            Ok ()
        | _ ->
            Error "expected first item to be an attribute item");
    Test.case "cst source files distinguish standalone extension items" (fun () ->
        let result =
          parse_ml "[%%toplevel_eval 42]\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.Extension extension :: _ ->
            Test.assert_equal ~expected:"EXTENSION_EXPR"
              ~actual:
                (SyntaxKind.to_string
                   (Ceibo.Red.SyntaxNode.kind extension.syntax_node));
            Test.assert_equal ~expected:"%"
              ~actual:(Syn.Cst.Token.text extension.sigil_token);
            Test.assert_equal ~expected:(Some "toplevel_eval")
              ~actual:(Syn.Cst.Ident.name extension.name);
            Test.assert_equal ~expected:None ~actual:extension.payload_syntax_node;
            (match extension.payload with
            | Some (Syn.Cst.Payload.Structure { item_syntax_nodes }) ->
                Test.assert_equal ~expected:1
                  ~actual:(List.length item_syntax_nodes);
                Ok ()
            | _ ->
                Error "expected structure payload for standalone extension item")
        | _ ->
            Error "expected first item to be an extension item");
    Test.case "cst interfaces distinguish standalone attribute items" (fun () ->
        let result = parse_mli "[@@@attr]\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.Attribute attribute :: _ ->
            Test.assert_equal ~expected:(Some "attr")
              ~actual:(Syn.Cst.Ident.name attribute.name);
            Ok ()
        | _ ->
            Error "expected first item to be an interface attribute item");
    Test.case "cst interfaces distinguish standalone extension items" (fun () ->
        let result = parse_mli "[%%signature_item]\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.Extension extension :: _ ->
            Test.assert_equal ~expected:(Some "signature_item")
              ~actual:(Syn.Cst.Ident.name extension.name);
            Ok ()
        | _ ->
            Error "expected first item to be an interface extension item");
    Test.case "cst interface extension payloads fall back to signature items"
      (fun () ->
        let result = parse_mli "[%%signature_item val x : int]\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        let payload_nodes =
          signature_items cst
          |> List.filter_map (function
               | Syn.Cst.SignatureItem.Extension
                   {
                     payload =
                       Some (Syn.Cst.Payload.Signature { item_syntax_nodes });
                     _;
                   } ->
                   Some item_syntax_nodes
               | Syn.Cst.SignatureItem.Extension
                   {
                     payload =
                       Some (Syn.Cst.Payload.Structure { item_syntax_nodes });
                     _;
                   } ->
                   Some item_syntax_nodes
               | _ ->
                   None)
        in
        match payload_nodes with
        | _ :: _ ->
            Ok ()
        | [] ->
            Error "expected interface extension payload");
    Test.case "cst attributed types keep attribute names and payload nodes" (fun () ->
        let result = parse_ml "type t = int [@foo]\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.TypeDeclaration
            {
              type_definition =
                Syn.Cst.TypeDefinition.Alias
                  { manifest = Syn.Cst.CoreType.Attribute { attribute; _ }; _ };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"@"
              ~actual:(Syn.Cst.Token.text attribute.sigil_token);
            Test.assert_equal ~expected:(Some "foo")
              ~actual:(Syn.Cst.Ident.name attribute.name);
            Test.assert_equal ~expected:None ~actual:attribute.payload_syntax_node;
            Test.assert_equal ~expected:None ~actual:attribute.payload;
            Ok ()
        | _ -> Error "expected attributed type alias");
    Test.case "cst pattern attributes attach orthogonally to the lifted pattern"
      (fun () ->
        let result = parse_ml "let (x [@foo]) = value\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              binding_pattern = Syn.Cst.Pattern.Parenthesized { inner; _ };
              _;
            }
          :: _ -> (
            match inner with
            | Syn.Cst.Pattern.Identifier { name_token; _ } ->
                let attributes = Syn.Cst.Pattern.attributes inner in
                Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text name_token);
                Test.assert_equal ~expected:1 ~actual:(List.length attributes);
                Test.assert_equal ~expected:[ Some "foo" ]
                  ~actual:
                    (attributes
                    |> List.map (fun ({ name; _ } : Syn.Cst.attribute) ->
                           Syn.Cst.Ident.name name));
                Ok ()
            | _ ->
                Error "expected parenthesized identifier pattern")
        | _ -> Error "expected let binding with parenthesized pattern");
    Test.case "cst expression attributes lift structure payloads and anchors"
      (fun () ->
        let result = parse_ml "let _ = value [@foo 1 + 2]\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Apply
                  {
                    argument =
                      Syn.Cst.Positional
                        (Syn.Cst.Expression.Attribute
                          {
                            payload_syntax_node = None;
                            payload =
                              Some
                                (Syn.Cst.Payload.Structure
                                  { item_syntax_nodes = item_node :: _ });
                            _;
                          });
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"INFIX_EXPR"
              ~actual:(SyntaxKind.to_string (Ceibo.Red.SyntaxNode.kind item_node));
            Ok ()
        | _ ->
            Error "expected expression attribute with structure payload");
    Test.case "cst extensions lift typed `:` payloads" (fun () ->
        let result = parse_ml "let _ = [%foo: int -> string]\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Extension
                  {
                    payload =
                      Some
                        (Syn.Cst.Payload.Type
                          (Syn.Cst.CoreType.Arrow
                            {
                              parameter_type =
                                Syn.Cst.CoreType.Constr { constructor_path = left; _ };
                              result_type =
                                Syn.Cst.CoreType.Constr { constructor_path = right; _ };
                              _;
                            }));
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name left);
            Test.assert_equal ~expected:(Some "string")
              ~actual:(Syn.Cst.Ident.name right);
            Ok ()
        | _ ->
            Error "expected typed extension payload");
    Test.case "cst exception declarations preserve declared names" (fun () ->
        let result = parse_ml "exception Not_found\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ExceptionDeclaration { name_token; _ } :: _ ->
            Test.assert_equal ~expected:"Not_found"
              ~actual:(Syn.Cst.Token.text name_token);
            Ok ()
        | _ -> Error "expected first item to be an exception declaration");
    Test.case "cst interface exception declarations preserve declared names"
      (fun () ->
        let result = parse_mli "exception Not_found\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.ExceptionDeclaration { name_token; _ } :: _ ->
            Test.assert_equal ~expected:"Not_found"
              ~actual:(Syn.Cst.Token.text name_token);
            Ok ()
        | _ ->
            Error "expected first item to be an interface exception declaration");
    Test.case "cst type declarations preserve first-class module type definitions"
      (fun () ->
        let result =
          parse_ml
            "type transport = (module Transport)\n\
             type driver = (module Driver with type config = int)\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | _first_decl
          :: Syn.Cst.StructureItem.TypeDeclaration
               {
                 type_definition =
                   Syn.Cst.TypeDefinition.FirstClassModule
                     {
                       module_type =
                         Syn.Cst.ModuleType.With
                           {
                             base = Syn.Cst.ModuleType.Path base_path;
                             constraints;
                             _;
                           };
                       _;
                     };
                 _;
               }
             :: _ ->
            Test.assert_equal ~expected:(Some "Driver")
              ~actual:(Syn.Cst.ModulePath.name base_path);
            Test.assert_equal ~expected:1 ~actual:(List.length constraints);
            Ok ()
        | _ -> Error "expected first-class module type definition");
    Test.case "cst type declarations distinguish class types from constructors"
      (fun () ->
        let result =
          parse_ml
            "type bare = #list\n\
             type applied = int #list\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.TypeDeclaration
            {
              type_definition =
                Syn.Cst.TypeDefinition.Alias
                  {
                    manifest =
                      Syn.Cst.CoreType.Class
                        {
                          hash_token;
                          class_path = bare_path;
                          arguments = [];
                          _;
                        };
                    _;
                  };
              _;
            }
          :: Syn.Cst.StructureItem.TypeDeclaration
               {
                 type_definition =
                   Syn.Cst.TypeDefinition.Alias
                     {
                       manifest =
                         Syn.Cst.CoreType.Class
                           {
                             class_path = applied_path;
                             arguments =
                               [
                                 Syn.Cst.CoreType.Constr
                                   {
                                     constructor_path = applied_argument;
                                     arguments = [];
                                     _;
                                   };
                               ];
                             _;
                           };
                       _;
                     };
                 _;
               }
             :: _ ->
            Test.assert_equal ~expected:"#"
              ~actual:(Syn.Cst.Token.text hash_token);
            Test.assert_equal ~expected:(Some "list")
              ~actual:(Syn.Cst.Ident.name bare_path);
            Test.assert_equal ~expected:(Some "list")
              ~actual:(Syn.Cst.Ident.name applied_path);
            Test.assert_equal ~expected:(Some "int")
              ~actual:(Syn.Cst.Ident.name applied_argument);
            Ok ()
        | _ -> Error "expected class-type aliases");
    Test.case "cst open statements preserve open! structurally" (fun () ->
        let result = parse_ml "open! Std.List\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.OpenStatement stmt :: _ ->
            Test.assert_true (Syn.Cst.OpenStatement.has_bang stmt);
            (match Syn.Cst.OpenStatement.target stmt with
            | Syn.Cst.OpenStatement.ModuleExpression
                (Syn.Cst.ModuleExpression.Path path) ->
                Test.assert_equal ~expected:(Some "List")
                  ~actual:(Syn.Cst.Ident.name path)
            | _ ->
                ());
            Test.assert_equal ~expected:(Some "List")
              ~actual:
                (match Syn.Cst.OpenStatement.module_path stmt with
                | Some module_path -> Syn.Cst.ModulePath.name module_path
                | None -> None);
            (match Syn.Cst.OpenStatement.target stmt with
            | Syn.Cst.OpenStatement.ModuleExpression
                (Syn.Cst.ModuleExpression.Path _) ->
                Ok ()
            | _ ->
                Error
                  "expected implementation open target to lift as a module expression")
        | _ -> Error "expected first item to be an open statement");
    Test.case "cst interface open statements preserve open! structurally"
      (fun () ->
        let result = parse_mli "open! Std.List\n" in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match signature_items cst with
        | Syn.Cst.SignatureItem.OpenStatement stmt :: _ ->
            Test.assert_true (Syn.Cst.OpenStatement.has_bang stmt);
            (match Syn.Cst.OpenStatement.target stmt with
            | Syn.Cst.OpenStatement.Path path ->
                Test.assert_equal ~expected:(Some "List")
                  ~actual:(Syn.Cst.Ident.name path)
            | _ ->
                ());
            Test.assert_equal ~expected:(Some "List")
              ~actual:
                (match Syn.Cst.OpenStatement.module_path stmt with
                | Some module_path -> Syn.Cst.ModulePath.name module_path
                | None -> None);
            (match Syn.Cst.OpenStatement.target stmt with
            | Syn.Cst.OpenStatement.Path _ ->
                Ok ()
            | _ ->
                Error "expected interface open target to remain a module path")
        | _ ->
            Error "expected first item to be an interface open statement");
    Test.case "cst implementation open statements lift non-path module expressions"
      (fun () ->
        let result =
          parse_ml
            "open struct\n\
             let value = 1\n\
             end\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.OpenStatement stmt :: _ -> (
            match Syn.Cst.OpenStatement.target stmt with
            | Syn.Cst.OpenStatement.ModuleExpression
                (Syn.Cst.ModuleExpression.Structure { item_syntax_nodes; _ }) ->
                Test.assert_true (List.length item_syntax_nodes > 0);
                Test.assert_equal ~expected:None
                  ~actual:(Syn.Cst.OpenStatement.module_path stmt);
                Ok ()
            | _ ->
                Error "expected implementation open target to preserve structure syntax")
        | _ ->
            Error "expected first item to be an implementation open statement");
    Test.case "cst source files preserve mixed structure item ordering" (fun () ->
        let source =
          "let first = 1\nmodule Middle = struct end\nlet second = 2\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding first
          :: Syn.Cst.StructureItem.ModuleDeclaration _
          :: Syn.Cst.StructureItem.LetBinding second
          :: _ ->
            Test.assert_equal ~expected:"first"
              ~actual:(Syn.Cst.LetBinding.name first);
            Test.assert_equal ~expected:"second"
              ~actual:(Syn.Cst.LetBinding.name second);
            Ok ()
        | _ ->
            Error "expected let/module/let item ordering");
    Test.case "cst let bindings expose typed parameters" (fun () ->
        let source =
          "let render userId ~displayName ?pageSize current_user = current_user\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match top_level_let_bindings cst with
        | binding :: _
          when String.equal (Syn.Cst.LetBinding.name binding) "render" ->
            let names =
              Syn.Cst.LetBinding.parameters binding
              |> List.map Syn.Cst.Parameter.name
            in
            Test.assert_equal
              ~expected:
                [ Some "userId"; Some "displayName"; Some "pageSize"; Some "current_user" ]
              ~actual:names;
            Ok ()
        | _ ->
            Error "expected render binding parameters");
    Test.case "cst let bindings preserve locally abstract type parameters"
      (fun () ->
        let source = "let id (type a b) value = value\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match top_level_let_bindings cst with
        | binding :: _ -> (
            match Syn.Cst.LetBinding.parameters binding with
            | Syn.Cst.Parameter.LocallyAbstract { binders; _ } :: _ ->
                let binder_text =
                  binders |> List.map Syn.Cst.TypeBinder.text
                in
                let quoted =
                  binders |> List.map Syn.Cst.TypeBinder.is_quoted
                in
                Test.assert_equal ~expected:[ "a"; "b" ] ~actual:binder_text;
                Test.assert_equal ~expected:[ false; false ] ~actual:quoted;
                Ok ()
            | _ -> Error "expected leading locally abstract type parameter")
        | [] -> Error "expected let binding");
    Test.case "cst let bindings preserve recursive markers" (fun () ->
        let source = "let rec loop x = loop x\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match top_level_let_bindings cst with
        | binding :: _ ->
            Test.assert_true (Syn.Cst.LetBinding.is_recursive binding);
            Ok ()
        | [] -> Error "expected recursive let binding");
    Test.case "cst let binding annotations wrap bound values as typed expressions"
      (fun () ->
        let source = "let render : user_t -> user_t = fun value -> value\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Typed
                  {
                    expression = Syn.Cst.Expression.Fun _;
                    type_ = Syn.Cst.CoreType.Arrow _;
                    _;
                  };
              _;
            }
          :: _ ->
            Ok ()
        | _ -> Error "expected typed let-binding value");
    Test.case "cst let binding annotations preserve explicit polymorphism"
      (fun () ->
        let source = "let id : 'a. 'a -> 'a = fun x -> x\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Polymorphic
                  {
                    expression =
                      Syn.Cst.Expression.Fun
                        {
                          body =
                            Syn.Cst.Expression
                              (Syn.Cst.Expression.Path { path; _ });
                          _;
                        };
                    type_ = Syn.Cst.CoreType.Poly { binders; _ };
                    _;
                  };
              _;
            }
          :: _ ->
            let binder_text =
              binders |> List.map Syn.Cst.TypeBinder.text
            in
            let quoted =
              binders |> List.map Syn.Cst.TypeBinder.is_quoted
            in
            Test.assert_equal ~expected:[ "'a" ] ~actual:binder_text;
            Test.assert_equal ~expected:[ true ] ~actual:quoted;
            Test.assert_equal ~expected:(Some "x")
              ~actual:(Syn.Cst.ModulePath.name path);
            Ok ()
        | _ -> Error "expected polymorphic let-binding value");
    Test.case "cst let bindings expose infix string concatenation values" (fun () ->
        let source = "let banner = \"a\" ^ \"b\" ^ \"c\"\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding binding :: _ -> (
            match Syn.Cst.LetBinding.value binding with
            | Syn.Cst.Expression.Infix expr ->
                Test.assert_equal ~expected:"^"
                  ~actual:(Syn.Cst.InfixExpression.operator expr);
                Ok ()
            | _ -> Error "expected infix expression value")
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst let bindings expose custom infix operators structurally" (fun () ->
        let source = "let composed = f %> g\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding binding :: _ -> (
            match Syn.Cst.LetBinding.value binding with
            | Syn.Cst.Expression.Infix expr ->
                Test.assert_equal ~expected:"%>"
                  ~actual:(Syn.Cst.InfixExpression.operator expr);
                Ok ()
            | _ -> Error "expected infix expression value")
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst let bindings expose if expressions and unit else branches" (fun () ->
        let source = "let render ok = if ok then log () else ()\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding binding :: _ -> (
            match Syn.Cst.LetBinding.value binding with
            | Syn.Cst.Expression.If expr -> (
                match expr.else_branch with
                | Some (Syn.Cst.Expression.Literal (Syn.Cst.Literal.Unit _)) -> Ok ()
                | _ -> Error "expected unit else branch")
            | _ -> Error "expected if expression value")
        | _ -> Error "expected first item to be a let binding");
    Test.case "cst if expressions preserve boolean literal comparisons" (fun () ->
        let source = "let render ok = if ok = true then log () else ()\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
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
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Typed
                  {
                    expression = Syn.Cst.Expression.Path { path; _ };
                    type_ = Syn.Cst.CoreType.Constr _;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "value")
              ~actual:(Syn.Cst.ModulePath.name path);
            Ok ()
        | _ -> Error "expected typed expression value");
    Test.case "cst coerce expressions preserve optional source types" (fun () ->
        let source = "let render = (value : user_t :> display_t)\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Coerce
                  {
                    expression = Syn.Cst.Expression.Path { path; _ };
                    from_type = Some (Syn.Cst.CoreType.Constr _);
                    to_type = Syn.Cst.CoreType.Constr _;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "value")
              ~actual:(Syn.Cst.ModulePath.name path);
            Ok ()
        | _ -> Error "expected coerce expression value");
    Test.case "cst source files keep top-level eval expressions as items" (fun () ->
        let source = "print_endline \"hello\"\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.Expression
            (Syn.Cst.Expression.Apply
              {
                callee =
                  Syn.Cst.Expression.Path
                    {
                      path = Syn.Cst.Ident.Ident { name_token; _ };
                      _;
                    };
                argument =
                  Syn.Cst.Positional
                    (Syn.Cst.Expression.Literal (Syn.Cst.Literal.String _));
                _;
              })
          :: _ ->
            Test.assert_equal ~expected:"print_endline"
              ~actual:(Syn.Cst.Token.text name_token);
            Ok ()
        | _ -> Error "expected top-level eval expression item");
    Test.case "cst source files keep top-level let-in expressions as items" (fun () ->
        let source = "let a, b = pair in a\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.Expression
            (Syn.Cst.Expression.Let
              {
                binding_pattern =
                  Syn.Cst.Pattern.Tuple
                    {
                      elements =
                        [
                          {
                            pattern =
                              Syn.Cst.Pattern.Identifier
                                { name_token = left_name; _ };
                            _;
                          };
                          {
                            pattern =
                              Syn.Cst.Pattern.Identifier
                                { name_token = right_name; _ };
                            _;
                          };
                        ];
                      open_tail = None;
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
    Test.case "cst tuple patterns preserve labeled payloads and open tails"
      (fun () ->
        let source =
          "let f value = match value with | ~state:Some x, ~rest, .. -> x | _ -> 0\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    cases =
                      {
                        pattern =
                          Syn.Cst.Pattern.Tuple
                            {
                              elements =
                                [
                                  {
                                    label_token = Some state_label;
                                    pattern =
                                      Syn.Cst.Pattern.Constructor
                                        {
                                          constructor_path =
                                            Syn.Cst.Ident.Ident
                                              { name_token = some_name; _ };
                                          arguments =
                                            [
                                              Syn.Cst.Pattern.Identifier
                                                { name_token = x_name; _ };
                                            ];
                                          _;
                                        };
                                  };
                                  {
                                    label_token = Some rest_label;
                                    pattern =
                                      Syn.Cst.Pattern.Identifier
                                        { name_token = rest_name; _ };
                                  };
                                ];
                              open_tail = Some { dotdot_token; _ };
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
            Test.assert_equal ~expected:"state"
              ~actual:(Syn.Cst.Token.text state_label);
            Test.assert_equal ~expected:"Some"
              ~actual:(Syn.Cst.Token.text some_name);
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text x_name);
            Test.assert_equal ~expected:"rest"
              ~actual:(Syn.Cst.Token.text rest_label);
            Test.assert_equal ~expected:"rest"
              ~actual:(Syn.Cst.Token.text rest_name);
            Test.assert_equal ~expected:".."
              ~actual:(Syn.Cst.Token.text dotdot_token);
            Ok ()
        | _ -> Error "expected labeled tuple pattern with open tail");
    Test.case "cst tuple patterns preserve typed labeled punning" (fun () ->
        let source = "let f = function | ~(x : int), y -> x | _ -> 0\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Function
                  {
                    cases =
                      {
                        pattern =
                          Syn.Cst.Pattern.Tuple
                            {
                              elements =
                                [
                                  {
                                    label_token = Some label_token;
                                    pattern =
                                      Syn.Cst.Pattern.Typed
                                        {
                                          pattern =
                                            Syn.Cst.Pattern.Identifier
                                              { name_token = x_name; _ };
                                          _;
                                        };
                                  };
                                  {
                                    label_token = None;
                                    pattern =
                                      Syn.Cst.Pattern.Identifier
                                        { name_token = y_name; _ };
                                  };
                                ];
                              open_tail = None;
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
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text label_token);
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text x_name);
            Test.assert_equal ~expected:"y"
              ~actual:(Syn.Cst.Token.text y_name);
            Ok ()
        | _ -> Error "expected typed labeled tuple pattern");
    Test.case "cst match cases preserve unreachable expressions" (fun () ->
        let source =
          "let absurd maybe = match maybe with | Some value -> value | None -> .\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    scrutinee = Syn.Cst.Expression.Path { path = scrutinee_path; _ };
                    cases =
                      [
                        _;
                        {
                          body =
                            Syn.Cst.Expression.Unreachable
                              { dot_token; _ };
                          _;
                        };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "maybe")
              ~actual:(Syn.Cst.ModulePath.name scrutinee_path);
            Test.assert_equal ~expected:"."
              ~actual:(Syn.Cst.Token.text dot_token);
            Ok ()
        | _ -> Error "expected match expression with unreachable branch");
    Test.case "cst let-operator expressions preserve the leading operator clause"
      (fun () ->
        let source =
          "let ( let* ) = Result.bind\nlet* value = Ok 1 in Ok value\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | _ :: Syn.Cst.StructureItem.Expression
                 (Syn.Cst.Expression.LetOperator
                   {
                     binding =
                       {
                         keyword_token;
                         operator_token;
                         binding_pattern =
                           Syn.Cst.Pattern.Identifier
                             { name_token = binding_name; _ };
                         bound_value = Syn.Cst.Expression.Constructor _;
                       };
                     and_bindings = [];
                     body = Syn.Cst.Expression.Constructor _;
                     _;
                   })
               :: _ ->
            Test.assert_equal ~expected:"let"
              ~actual:(Syn.Cst.Token.text keyword_token);
            Test.assert_equal ~expected:"*"
              ~actual:(Syn.Cst.Token.text operator_token);
            Test.assert_equal ~expected:"value"
              ~actual:(Syn.Cst.Token.text binding_name);
            Ok ()
        | _ -> Error "expected top-level let-operator expression item");
    Test.case "cst let-operator expressions preserve parallel and-bindings"
      (fun () ->
        let source =
          "let ( let* ) = Result.bind\n\
           let ( and* ) = Result.both\n\
           let* a = Ok 1 and* b = Ok 2 in Ok (a, b)\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | _ :: _ :: Syn.Cst.StructureItem.Expression
                     (Syn.Cst.Expression.LetOperator
                       {
                         binding =
                           {
                             keyword_token = let_keyword;
                             operator_token = let_operator;
                             binding_pattern =
                               Syn.Cst.Pattern.Identifier
                                 { name_token = left_name; _ };
                             bound_value = Syn.Cst.Expression.Constructor _;
                           };
                         and_bindings =
                           [
                             {
                               keyword_token = and_keyword;
                               operator_token = and_operator;
                               binding_pattern =
                                 Syn.Cst.Pattern.Identifier
                                   { name_token = right_name; _ };
                               bound_value = Syn.Cst.Expression.Constructor _;
                             };
                           ];
                         body = Syn.Cst.Expression.Constructor _;
                         _;
                       })
               :: _ ->
            Test.assert_equal ~expected:"let"
              ~actual:(Syn.Cst.Token.text let_keyword);
            Test.assert_equal ~expected:"*"
              ~actual:(Syn.Cst.Token.text let_operator);
            Test.assert_equal ~expected:"a"
              ~actual:(Syn.Cst.Token.text left_name);
            Test.assert_equal ~expected:"and"
              ~actual:(Syn.Cst.Token.text and_keyword);
            Test.assert_equal ~expected:"*"
              ~actual:(Syn.Cst.Token.text and_operator);
            Test.assert_equal ~expected:"b"
              ~actual:(Syn.Cst.Token.text right_name);
            Ok ()
        | _ -> Error "expected parallel let-operator expression item");
    Test.case "cst let expressions expose unit-pattern sequencing structurally" (fun () ->
        let source = "let render () = let () = log () in flush ()\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Let
                  {
                    binding_pattern =
                      Syn.Cst.Pattern.Literal
                        { literal = Syn.Cst.PatternLiteral.Unit _; _ };
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
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Fun
                  {
                    parameters = [ param ];
                    body =
                      Syn.Cst.Expression
                        (Syn.Cst.Expression.Path { path; _ });
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
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value = Syn.Cst.Expression.Function { cases; _ };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:2 ~actual:(List.length cases);
            Ok ()
        | _ -> Error "expected first item to be a let binding with a function expression");
    Test.case "cst fun expressions preserve nested function case bodies"
      (fun () ->
        let source =
          "let render default = fun value -> function | Some current -> current | None -> default\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Fun
                  {
                    parameters = [ param ];
                    body =
                      Syn.Cst.Cases
                        {
                          cases =
                            {
                              pattern =
                                Syn.Cst.Pattern.Constructor
                                  {
                                    constructor_path = some_path;
                                    arguments =
                                      [
                                        Syn.Cst.Pattern.Identifier
                                          { name_token = current_name; _ };
                                      ];
                                    _;
                                  };
                              body =
                                Syn.Cst.Expression.Path
                                  { path = current_path; _ };
                              _;
                            }
                            :: {
                                 pattern =
                                   Syn.Cst.Pattern.Constructor
                                     {
                                       constructor_path = none_path;
                                       arguments = [];
                                       _;
                                     };
                                 body =
                                   Syn.Cst.Expression.Path
                                     { path = default_path; _ };
                                 _;
                               }
                            :: _;
                          _;
                        };
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "value")
              ~actual:(Syn.Cst.Parameter.name param);
            Test.assert_equal ~expected:(Some "Some")
              ~actual:(Syn.Cst.ModulePath.name some_path);
            Test.assert_equal ~expected:"current"
              ~actual:(Syn.Cst.Token.text current_name);
            Test.assert_equal ~expected:(Some "current")
              ~actual:(Syn.Cst.ModulePath.name current_path);
            Test.assert_equal ~expected:(Some "None")
              ~actual:(Syn.Cst.ModulePath.name none_path);
            Test.assert_equal ~expected:(Some "default")
              ~actual:(Syn.Cst.ModulePath.name default_path);
            Ok ()
        | _ -> Error "expected fun expression with nested function body");
    Test.case "cst match expressions expose boolean cases structurally" (fun () ->
        let source = "let render flag = match flag with true -> 1 | false -> 0\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    cases =
                      [
                        {
                          pattern =
                            Syn.Cst.Pattern.Literal
                              {
                                literal =
                                  Syn.Cst.PatternLiteral.Bool
                                    { literal_token = first; _ };
                                _;
                              };
                          body = _;
                          _;
                        };
                        {
                          pattern =
                            Syn.Cst.Pattern.Literal
                              {
                                literal =
                                  Syn.Cst.PatternLiteral.Bool
                                    { literal_token = second; _ };
                                _;
                              };
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
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
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
    Test.case "cst try expressions expose effect handlers structurally" (fun () ->
        let source =
          "let render thunk = try thunk () with | effect (Yield x), k -> continue k x\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Try
                  {
                    cases =
                      [
                        {
                          pattern =
                            Syn.Cst.Pattern.Effect
                              {
                                effect_pattern =
                                  Syn.Cst.Pattern.Parenthesized
                                    {
                                      inner =
                                        Syn.Cst.Pattern.Constructor
                                          {
                                            constructor_path =
                                              Syn.Cst.Ident.Ident
                                                {
                                                  name_token = effect_name;
                                                  _;
                                                };
                                            arguments =
                                              [
                                                Syn.Cst.Pattern.Identifier
                                                  {
                                                    name_token = effect_value;
                                                    _;
                                                  };
                                              ];
                                            _;
                                          };
                                      _;
                                    };
                                continuation =
                                  Syn.Cst.Pattern.Identifier
                                    { name_token = continuation; _ };
                                _;
                              };
                          body = Syn.Cst.Expression.Apply _;
                          _;
                        };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"Yield"
              ~actual:(Syn.Cst.Token.text effect_name);
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text effect_value);
            Test.assert_equal ~expected:"k"
              ~actual:(Syn.Cst.Token.text continuation);
            Ok ()
        | _ -> Error "expected first item to be a let binding with an effect handler");
    Test.case "cst let bindings preserve infix expressions structurally" (fun () ->
        let source = "let changed = (left <> right)\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match top_level_let_bindings cst with
        | binding :: _ -> (
            let value =
              match Syn.Cst.LetBinding.value binding with
              | Syn.Cst.Expression.Parenthesized { inner; _ } -> inner
              | expr -> expr
            in
            match value with
            | Syn.Cst.Expression.Infix expr ->
                Test.assert_equal ~expected:"<>"
                  ~actual:(Syn.Cst.InfixExpression.operator expr);
                Ok ()
            | _ ->
                Error "expected bound value to contain an infix expression")
        | [] ->
            Error "expected let binding");
    Test.case "cst let bindings expose apply and field access expressions structurally" (fun () ->
        let source = "let reversed = List.rev (List.rev xs)\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding binding :: _ -> (
            match Syn.Cst.LetBinding.value binding with
            | Syn.Cst.Expression.Apply outer -> (
                match outer with
                | {
                 callee =
                   Syn.Cst.Expression.FieldAccess
                     {
                       receiver = Syn.Cst.Expression.Path { path; _ };
                       field_name;
                       _;
                     };
                 argument =
                   Syn.Cst.Positional
                     (Syn.Cst.Expression.Parenthesized
                       {
                         inner = Syn.Cst.Expression.Apply _;
                         _;
                       });
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
    Test.case "cst constructor expressions preserve bare and applied forms"
      (fun () ->
        let source =
          "let some = Some 42\n\
           let none = None\n\
           let err = Result.Error message\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match top_level_let_bindings cst with
        | some_binding :: none_binding :: err_binding :: _ -> (
            match
              ( Syn.Cst.LetBinding.value some_binding,
                Syn.Cst.LetBinding.value none_binding,
                Syn.Cst.LetBinding.value err_binding )
            with
            | ( Syn.Cst.Expression.Constructor
                  { constructor_path = some_path; payload = Some payload; _ },
                Syn.Cst.Expression.Constructor
                  { constructor_path = none_path; payload = None; _ },
                Syn.Cst.Expression.Constructor
                  { constructor_path = err_path; payload = Some err_payload; _ }
              ) ->
                Test.assert_equal ~expected:(Some "Some")
                  ~actual:(Syn.Cst.ModulePath.name some_path);
                Test.assert_equal ~expected:(Some "None")
                  ~actual:(Syn.Cst.ModulePath.name none_path);
                Test.assert_equal ~expected:(Some "Error")
                  ~actual:(Syn.Cst.ModulePath.name err_path);
                (match payload with
                | Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int _) -> (
                    match err_payload with
                    | Syn.Cst.Expression.Path { path; _ } ->
                        Test.assert_equal ~expected:(Some "message")
                          ~actual:(Syn.Cst.ModulePath.name path);
                        Ok ()
                    | _ ->
                        Error
                          "expected qualified constructor payload to remain path")
                | _ -> Error "expected constructor payload to remain literal")
            | _ -> Error "expected constructor-shaped expression values")
        | _ -> Error "expected three let bindings");
    Test.case "cst apply expressions preserve labeled arguments structurally"
      (fun () ->
        let source = "let x = f ~y:1\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Apply
                  {
                    callee = Syn.Cst.Expression.Path _;
                    argument =
                      Syn.Cst.Labeled
                        {
                          label_token;
                          value =
                            Some
                              (Syn.Cst.Expression.Literal
                                (Syn.Cst.Literal.Int _));
                          _;
                        };
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"y"
              ~actual:(Syn.Cst.Token.text label_token);
            Ok ()
        | _ -> Error "expected labeled apply argument");
    Test.case "cst apply expressions preserve optional shorthand arguments"
      (fun () ->
        let source = "let x = f ?y\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Apply
                  {
                    argument =
                      Syn.Cst.Optional
                        {
                          label_token;
                          value = None;
                          _;
                        };
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"y"
              ~actual:(Syn.Cst.Token.text label_token);
            Ok ()
        | _ -> Error "expected optional shorthand apply argument");
    Test.case "cst local opens preserve module paths from token-only syntax"
      (fun () ->
        let source = "let x =\n  let open List in\n  map f xs\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.LocalOpen
                  {
                    module_path = Syn.Cst.ModulePath.Ident { name_token = module_name; _ };
                    body = Syn.Cst.Expression.Apply _;
                    via_let_open = true;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"List"
              ~actual:(Syn.Cst.Token.text module_name);
            Ok ()
        | _ -> Error "expected local open expression");
    Test.case "cst prefix local opens preserve module paths and body expressions"
      (fun () ->
        let source = "let x = M.{ field = 42 }\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.LocalOpen
                  {
                    module_path = Syn.Cst.ModulePath.Ident { name_token = module_name; _ };
                    body =
                      Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal _);
                    via_let_open = false;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"M"
              ~actual:(Syn.Cst.Token.text module_name);
            Ok ()
        | _ -> Error "expected prefix local open expression");
    Test.case "cst local open patterns preserve module paths and wrapped patterns"
      (fun () ->
        let source =
          "let unwrap = function\n| Outer.Inner.(Some x) -> x\n| Outer.Inner.(None) -> 0\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Function
                  {
                    cases =
                      {
                        pattern =
                          Syn.Cst.Pattern.LocalOpen
                            {
                              module_path =
                                Syn.Cst.ModulePath.Qualified
                                  {
                                    prefix =
                                      Syn.Cst.ModulePath.Ident
                                        { name_token = outer_module; _ };
                                    name_token = inner_module;
                                    _;
                                  };
                              pattern =
                                Syn.Cst.Pattern.Constructor
                                  {
                                    constructor_path =
                                      Syn.Cst.Ident.Ident
                                        { name_token = constructor_name; _ };
                                    arguments =
                                      [
                                        Syn.Cst.Pattern.Identifier
                                          { name_token = binding_name; _ };
                                      ];
                                    _;
                                  };
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
            Test.assert_equal ~expected:"Outer"
              ~actual:(Syn.Cst.Token.text outer_module);
            Test.assert_equal ~expected:"Inner"
              ~actual:(Syn.Cst.Token.text inner_module);
            Test.assert_equal ~expected:"Some"
              ~actual:(Syn.Cst.Token.text constructor_name);
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text binding_name);
            Ok ()
        | _ -> Error "expected local open pattern");
    Test.case "cst first-class module patterns preserve anonymous unpack binders"
      (fun () ->
        let source =
          "let ignore_typed packed = let (module _ : S) = packed in ()\n\
           let ignore_plain packed = let (module _) = packed in ()\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match top_level_let_bindings cst with
        | { value =
              Syn.Cst.Expression.Let
                {
                  binding_pattern =
                    Syn.Cst.Pattern.FirstClassModule
                      {
                        binding = Syn.Cst.Anonymous { wildcard_token = typed_wildcard };
                        module_type = Some (Syn.Cst.ModuleType.Path typed_module_type);
                        _;
                      };
                  _;
                };
            _;
          }
          :: { value =
                 Syn.Cst.Expression.Let
                   {
                     binding_pattern =
                       Syn.Cst.Pattern.FirstClassModule
                         {
                           binding = Syn.Cst.Anonymous { wildcard_token = plain_wildcard };
                           module_type = None;
                           _;
                         };
                     _;
                   };
               _;
             }
          :: _ ->
            Test.assert_equal ~expected:"_"
              ~actual:(Syn.Cst.Token.text typed_wildcard);
            Test.assert_equal ~expected:"_"
              ~actual:(Syn.Cst.Token.text plain_wildcard);
            Test.assert_equal ~expected:(Some "S")
              ~actual:(Syn.Cst.ModulePath.name typed_module_type);
            Ok ()
        | _ -> Error "expected anonymous first-class module patterns");
    Test.case "cst first-class module expressions preserve module and type nodes"
      (fun () ->
        let source = "let x = (module M : S)\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.FirstClassModule
                  {
                    module_expression = Syn.Cst.ModuleExpression.Path module_path;
                    module_type = Some (Syn.Cst.ModuleType.Path module_type_path);
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "M")
              ~actual:(Syn.Cst.ModulePath.name module_path);
            Test.assert_equal ~expected:(Some "S")
              ~actual:(Syn.Cst.ModulePath.name module_type_path);
            Ok ()
        | _ -> Error "expected first-class module expression");
    Test.case "cst qualified module paths preserve recursive structure" (fun () ->
        let source = "let x = (module Std.Net.TcpClient : Std.Net.Transport)\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.FirstClassModule
                  {
                    module_expression =
                      Syn.Cst.ModuleExpression.Path
                        (Syn.Cst.ModulePath.Qualified
                          {
                            prefix =
                              Syn.Cst.ModulePath.Qualified
                                {
                                  prefix = Syn.Cst.ModulePath.Ident { name_token = root; _ };
                                  name_token = mid;
                                  _;
                                };
                            name_token = leaf;
                            _;
                          });
                    module_type =
                      Some
                        (Syn.Cst.ModuleType.Path
                          (Syn.Cst.ModulePath.Qualified
                            {
                              prefix =
                                Syn.Cst.ModulePath.Qualified
                                  {
                                    prefix = Syn.Cst.ModulePath.Ident { name_token = type_root; _ };
                                    name_token = type_mid;
                                    _;
                                  };
                              name_token = type_leaf;
                              _;
                            }));
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"Std" ~actual:(Syn.Cst.Token.text root);
            Test.assert_equal ~expected:"Net" ~actual:(Syn.Cst.Token.text mid);
            Test.assert_equal ~expected:"TcpClient"
              ~actual:(Syn.Cst.Token.text leaf);
            Test.assert_equal ~expected:"Std" ~actual:(Syn.Cst.Token.text type_root);
            Test.assert_equal ~expected:"Net" ~actual:(Syn.Cst.Token.text type_mid);
            Test.assert_equal ~expected:"Transport"
              ~actual:(Syn.Cst.Token.text type_leaf);
            Ok ()
        | _ -> Error "expected qualified first-class module path");
    Test.case
      "cst first-class module expressions preserve structured packed payloads"
      (fun () ->
        let source = "let x = (module struct let y = 1 end : S)\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.FirstClassModule
                  {
                    module_expression =
                      Syn.Cst.ModuleExpression.Structure
                        { item_syntax_nodes = [ item_node ]; _ };
                    module_type = Some (Syn.Cst.ModuleType.Path module_type_path);
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:SyntaxKind.LET_BINDING
              ~actual:(Ceibo.Red.SyntaxNode.kind item_node);
            Test.assert_equal ~expected:(Some "S")
              ~actual:(Syn.Cst.ModulePath.name module_type_path);
            Ok ()
        | _ -> Error "expected structured packed first-class module expression");
    Test.case "cst module type declarations preserve functor module type bodies"
      (fun () ->
        let result =
          parse_ml "module type F = functor (X : S) -> T\n"
        in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.ModuleTypeDeclaration
            {
              module_type =
                Some
                  (Syn.Cst.ModuleType.Functor
                    {
                      parameters =
                        [ { name_token; module_type = Syn.Cst.ModuleType.Path param_type; _ } ];
                      result = Syn.Cst.ModuleType.Path result_type;
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"X"
              ~actual:(Syn.Cst.Token.text name_token);
            Test.assert_equal ~expected:(Some "S")
              ~actual:(Syn.Cst.ModulePath.name param_type);
            Test.assert_equal ~expected:(Some "T")
              ~actual:(Syn.Cst.ModulePath.name result_type);
            Ok ()
        | _ -> Error "expected module type declaration with functor body");
    Test.case "cst let-module expressions preserve module name and body"
      (fun () ->
        let source = "let run driver = let module D = (val driver) in D.execute ()\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.LetModule
                  {
                    module_name_token;
                    module_expression =
                      Syn.Cst.ModuleExpression.Unpack
                        {
                          expression =
                            Syn.Cst.Expression.Path { path = module_path; _ };
                          module_type = None;
                          _;
                        };
                    body = Syn.Cst.Expression.Apply _;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"D"
              ~actual:(Syn.Cst.Token.text module_name_token);
            Test.assert_equal ~expected:(Some "driver")
              ~actual:(Syn.Cst.ModulePath.name module_path);
            Ok ()
        | _ -> Error "expected let-module expression");
    Test.case "cst field access preserves nested qualified field access structurally" (fun () ->
        let source = "let render record = record.Module.field\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
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
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding binding :: _ ->
            let rec depth = function
              | Syn.Cst.Expression.Parenthesized expr ->
                  1 + depth expr.inner
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
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
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
                                                {
                                                  pattern =
                                                    Syn.Cst.Pattern.Identifier
                                                      { name_token = head_name; _ };
                                                  _;
                                                };
                                                {
                                                  pattern = Syn.Cst.Pattern.Wildcard _;
                                                  _;
                                                };
                                              ];
                                            open_tail = None;
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
    Test.case "cst constructor patterns preserve existential binders"
      (fun () ->
        let source =
          "let unwrap = function | Pair (type a b) ((left, right) : a * b) -> left\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Function
                  {
                    cases =
                      {
                        pattern =
                          Syn.Cst.Pattern.Constructor
                            {
                              constructor_path;
                              existentials =
                                Some
                                  {
                                    binders =
                                      [
                                        Syn.Cst.TypeBinder.Bare
                                          { name_token = a_name };
                                        Syn.Cst.TypeBinder.Bare
                                          { name_token = b_name };
                                      ];
                                    _;
                                  };
                              arguments = [ Syn.Cst.Pattern.Typed _ ];
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
            Test.assert_equal ~expected:(Some "Pair")
              ~actual:(Syn.Cst.ModulePath.name constructor_path);
            Test.assert_equal ~expected:"a"
              ~actual:(Syn.Cst.Token.text a_name);
            Test.assert_equal ~expected:"b"
              ~actual:(Syn.Cst.Token.text b_name);
            Ok ()
        | _ -> Error "expected constructor existential binders");
    Test.case "cst match cases preserve alias and typed patterns structurally"
      (fun () ->
        let source =
          "let render value = match value with | (user : user_t) as current_user -> current_user\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
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
                                    type_ = Syn.Cst.CoreType.Constr _;
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
            Ok ()
        | _ -> Error "expected faithful alias typed pattern structure");
    Test.case "cst lazy patterns preserve the wrapped pattern" (fun () ->
        let source = "let f x = match x with | (lazy y) -> y\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    cases =
                      [
                        {
                          pattern =
                            Syn.Cst.Pattern.Parenthesized
                              {
                                inner =
                                  Syn.Cst.Pattern.Lazy
                                    {
                                      pattern =
                                        Syn.Cst.Pattern.Identifier { name_token; _ };
                                      _;
                                    };
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
            Test.assert_equal ~expected:"y"
              ~actual:(Syn.Cst.Token.text name_token);
            Ok ()
        | _ -> Error "expected lazy pattern structure");
    Test.case "cst exception patterns preserve the wrapped constructor" (fun () ->
        let source =
          "let f x = match x with exception Not_found -> None | y -> Some y\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    cases =
                      {
                        pattern =
                          Syn.Cst.Pattern.Exception
                            {
                              pattern =
                                Syn.Cst.Pattern.Constructor { constructor_path; _ };
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
            Test.assert_equal ~expected:(Some "Not_found")
              ~actual:(Syn.Cst.ModulePath.name constructor_path);
            Ok ()
        | _ -> Error "expected exception pattern structure");
    Test.case "cst range patterns preserve the written bounds" (fun () ->
        let source =
          "let f x = match x with | 'a' .. 'z' -> \"lowercase\" | _ -> \"other\"\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    cases =
                      {
                        pattern =
                          Syn.Cst.Pattern.Range
                            {
                              lower =
                                Syn.Cst.PatternLiteral.Char
                                  { literal_token = lower_token; contents = lower_contents; _ };
                              upper =
                                Syn.Cst.PatternLiteral.Char
                                  { literal_token = upper_token; contents = upper_contents; _ };
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
            Test.assert_equal ~expected:"'a'"
              ~actual:(Syn.Cst.Token.text lower_token);
            Test.assert_equal ~expected:"a" ~actual:lower_contents;
            Test.assert_equal ~expected:"'z'"
              ~actual:(Syn.Cst.Token.text upper_token);
            Test.assert_equal ~expected:"z" ~actual:upper_contents;
            Ok ()
        | _ -> Error "expected range pattern structure");
    Test.case "cst literals preserve structured constant details" (fun () ->
        let source = "let x = 0xffL\nlet y = 1.2g\nlet z = {|hello|}\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Literal
                  (Syn.Cst.Literal.Int
                    { base = Syn.Cst.Hexadecimal; prefix; digits; suffix; _ });
              _;
            }
          :: Syn.Cst.StructureItem.LetBinding
               {
                 value =
                   Syn.Cst.Expression.Literal
                     (Syn.Cst.Literal.Float
                       {
                         integral_digits;
                         fractional_digits;
                         exponent = None;
                         suffix = Some float_suffix;
                         _;
                       });
                 _;
               }
             :: Syn.Cst.StructureItem.LetBinding
                  {
                    value =
                      Syn.Cst.Expression.Literal
                        (Syn.Cst.Literal.String
                          {
                            delimiter = Syn.Cst.Quoted { marker = string_marker };
                            contents;
                            terminated;
                            _;
                          });
                    _;
                  }
                :: _ ->
            Test.assert_equal ~expected:(Some "0x") ~actual:prefix;
            Test.assert_equal ~expected:"ff" ~actual:digits;
            Test.assert_equal ~expected:(Some "L") ~actual:suffix;
            Test.assert_equal ~expected:"1" ~actual:integral_digits;
            Test.assert_equal ~expected:"2" ~actual:fractional_digits;
            Test.assert_equal ~expected:"g" ~actual:float_suffix;
            Test.assert_equal ~expected:"" ~actual:string_marker;
            Test.assert_equal ~expected:"hello" ~actual:contents;
            Test.assert_true terminated;
            Ok ()
        | _ -> Error "expected structured literal constants");
    Test.case "cst record expressions preserve literal fields structurally" (fun () ->
        let source = "let point = { x = 1; y = 2 }\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Record
                  (Syn.Cst.RecordExpression.Literal
                    {
                      fields =
                        [
                          {
                            field_path = first;
                            field_name = first_name;
                            value = Syn.Cst.Expression.Literal _;
                            source = Syn.Cst.Explicit;
                            _;
                          };
                          {
                            field_path = second;
                            field_name = second_name;
                            value = Syn.Cst.Expression.Literal _;
                            source = Syn.Cst.Explicit;
                            _;
                          };
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
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text first_name);
            Test.assert_equal ~expected:"y"
              ~actual:(Syn.Cst.Token.text second_name);
            Ok ()
        | _ -> Error "expected literal record expression");
    Test.case "cst record expressions preserve punning as explicit path values"
      (fun () ->
        let source = "let make ~x ~y = { x; y }\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Record
                  (Syn.Cst.RecordExpression.Literal
                    {
                      fields =
                        [
                          {
                            field_path = first;
                            field_name = first_name;
                            value = Syn.Cst.Expression.Path { path = first_value; _ };
                            source = Syn.Cst.Punned;
                            _;
                          };
                          {
                            field_path = second;
                            field_name = second_name;
                            value = Syn.Cst.Expression.Path { path = second_value; _ };
                            source = Syn.Cst.Punned;
                            _;
                          };
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
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text first_name);
            Test.assert_equal ~expected:"y"
              ~actual:(Syn.Cst.Token.text second_name);
            Test.assert_equal ~expected:(Some "x")
              ~actual:(Syn.Cst.ModulePath.name first_value);
            Test.assert_equal ~expected:(Some "y")
              ~actual:(Syn.Cst.ModulePath.name second_value);
            Ok ()
        | _ -> Error "expected punned record expression");
    Test.case "cst record update expressions preserve base and updated fields"
      (fun () ->
        let source = "let point = { point with x = 3 }\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Record
                  (Syn.Cst.RecordExpression.Update
                    {
                      base = Syn.Cst.Expression.Path { path = base_path; _ };
                      fields =
                        [
                          {
                            field_path;
                            field_name;
                            value = Syn.Cst.Expression.Literal _;
                            source = Syn.Cst.Explicit;
                            _;
                          };
                        ];
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "point")
              ~actual:(Syn.Cst.ModulePath.name base_path);
            Test.assert_equal ~expected:(Some "x")
              ~actual:(Syn.Cst.ModulePath.name field_path);
            Test.assert_equal ~expected:"x"
              ~actual:(Syn.Cst.Token.text field_name);
            Ok ()
        | _ -> Error "expected update record expression");
    Test.case "cst object override expressions preserve instance variables"
      (fun () ->
        let source = "let next = {< current = state; count = total >}\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.ObjectOverride
                  {
                    fields =
                      [
                        {
                          field_name = current_name;
                          value =
                            Some (Syn.Cst.Expression.Path { path = current_value; _ });
                          _;
                        };
                        {
                          field_name = count_name;
                          value =
                            Some (Syn.Cst.Expression.Path { path = count_value; _ });
                          _;
                        };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"current"
              ~actual:(Syn.Cst.Token.text current_name);
            Test.assert_equal ~expected:(Some "state")
              ~actual:(Syn.Cst.ModulePath.name current_value);
            Test.assert_equal ~expected:"count"
              ~actual:(Syn.Cst.Token.text count_name);
            Test.assert_equal ~expected:(Some "total")
              ~actual:(Syn.Cst.ModulePath.name count_value);
            Ok ()
        | _ -> Error "expected object override expression");
    Test.case "cst index and assign expressions preserve the written target"
      (fun () ->
        let source = "let x = arr.(0) <- 5\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
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
    Test.case "cst field assignments preserve field-access targets" (fun () ->
        let source = "let () = obj.field <- 10\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.FieldAssign
                  {
                    target =
                      {
                        receiver = Syn.Cst.Expression.Path { path; _ };
                        field_name;
                        _;
                      };
                    operator_token;
                    value = Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int _);
                    _;
                  };
                _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "obj")
              ~actual:(Syn.Cst.ModulePath.name path);
            Test.assert_equal ~expected:"field"
              ~actual:(Syn.Cst.Token.text field_name);
            Test.assert_equal ~expected:"<-"
              ~actual:(Syn.Cst.Token.text operator_token);
            Ok ()
        | _ -> Error "expected field assignment expression");
    Test.case "cst object methods preserve instance-variable assignments"
      (fun () ->
        let source =
          "let counter =\n  object\n    val mutable count = 0\n    method set next = count <- next\n  end\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Object
                  {
                    members =
                      [
                        Syn.Cst.ObjectMember.Value _;
                        Syn.Cst.ObjectMember.Method
                          {
                            body =
                              Some
                                (Syn.Cst.Expression.InstanceVariableAssign
                                  {
                                    name_token;
                                    operator_token;
                                    value =
                                      Syn.Cst.Expression.Path { path = value_path; _ };
                                    _;
                                  });
                            _;
                          };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"count"
              ~actual:(Syn.Cst.Token.text name_token);
            Test.assert_equal ~expected:"<-"
              ~actual:(Syn.Cst.Token.text operator_token);
            Test.assert_equal ~expected:(Some "next")
              ~actual:(Syn.Cst.ModulePath.name value_path);
            Ok ()
        | _ -> Error "expected object method instance-variable assignment");
    Test.case "cst object expressions preserve extension members" (fun () ->
        let source =
          "let value =\n  object\n    [%%foo]\n    method run = 1\n  end\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Object
                  {
                    members =
                      [
                        Syn.Cst.ObjectMember.Extension extension;
                        Syn.Cst.ObjectMember.Method
                          {
                            name_token;
                            body =
                              Some
                                (Syn.Cst.Expression.Literal
                                  (Syn.Cst.Literal.Int { literal_token; _ }));
                            _;
                          };
                      ];
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"%"
              ~actual:(Syn.Cst.Token.text extension.sigil_token);
            Test.assert_equal ~expected:(Some "foo")
              ~actual:(Syn.Cst.Ident.name extension.name);
            Test.assert_equal ~expected:"run"
              ~actual:(Syn.Cst.Token.text name_token);
            Test.assert_equal ~expected:"1"
              ~actual:(Syn.Cst.Token.text literal_token);
            Ok ()
        | _ -> Error "expected object extension member");
    Test.case "cst record patterns preserve field punning and nested patterns"
      (fun () ->
        let source = "let x = match r with { user = { id }; name } -> id\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
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
                                              closedness = Syn.Cst.Closed;
                                              _;
                                            });
                                      _;
                                    };
                                    { field_path = name_field; pattern = None; _ };
                                  ];
                                closedness = Syn.Cst.Closed;
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
    Test.case "cst record patterns preserve open wildcard tails" (fun () ->
        let source = "let x = match r with { user; _ } -> user\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
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
                                  [ { field_path; pattern = None; _ } ];
                                closedness = Syn.Cst.Open { wildcard_token };
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
              ~actual:(Syn.Cst.ModulePath.name field_path);
            Test.assert_equal ~expected:"_"
              ~actual:(Syn.Cst.Token.text wildcard_token);
            Ok ()
        | _ -> Error "expected open record pattern");
    Test.case "cst array patterns preserve literal element patterns" (fun () ->
        let source = "let x = match xs with [| 1; value |] -> value\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
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
                                      { literal = Syn.Cst.PatternLiteral.Int _; _ };
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
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
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
    Test.case "cst polyvariant expressions preserve tags and payloads" (fun () ->
        let source = "let x = `Point { y = 1; z = 2 }\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.PolyVariant
                  {
                    tag_token;
                    payload =
                      Some
                        (Syn.Cst.Expression.Record
                          (Syn.Cst.RecordExpression.Literal { fields; _ }));
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"Point"
              ~actual:(Syn.Cst.Token.text tag_token);
            Test.assert_equal ~expected:2 ~actual:(List.length fields);
            Ok ()
        | _ -> Error "expected polyvariant expression");
    Test.case "cst polyvariant patterns mirror bare and payload parsetree forms"
      (fun () ->
        let source =
          "let x = match y with `Done -> 0 | `Point (a, b) -> a\n"
        in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Match
                  {
                    cases =
                      [
                        {
                          pattern =
                            Syn.Cst.Pattern.PolyVariant
                              { tag_token = bare_tag_token; payload = None; _ };
                          _;
                        };
                        {
                          pattern =
                            Syn.Cst.Pattern.PolyVariant
                              {
                                tag_token;
                                payload =
                                  Some
                                    (Syn.Cst.Pattern.Parenthesized
                                      {
                                        inner =
                                          Syn.Cst.Pattern.Tuple
                                            {
                                              elements =
                                                [
                                                  {
                                                    pattern =
                                                      Syn.Cst.Pattern.Identifier
                                                        { name_token; _ };
                                                    _;
                                                  };
                                                  _;
                                                ];
                                              open_tail = None;
                                              _;
                                            };
                                        _;
                                      });
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
            Test.assert_equal ~expected:"Done"
              ~actual:(Syn.Cst.Token.text bare_tag_token);
            Test.assert_equal ~expected:"Point"
              ~actual:(Syn.Cst.Token.text tag_token);
            Test.assert_equal ~expected:"a"
              ~actual:(Syn.Cst.Token.text name_token);
            Ok ()
        | _ -> Error "expected polyvariant pattern");
    Test.case "cst nested record updates preserve expression bases" (fun () ->
        let source = "let x = { { point with a = 1 } with b = 2 }\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Record
                  (Syn.Cst.RecordExpression.Update
                    {
                      base =
                        Syn.Cst.Expression.Record
                          (Syn.Cst.RecordExpression.Update
                            {
                              base = Syn.Cst.Expression.Path { path; _ };
                              _;
                            });
                      fields = [ { field_path; _ } ];
                      _;
                    });
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:(Some "point")
              ~actual:(Syn.Cst.ModulePath.name path);
            Test.assert_equal ~expected:(Some "b")
              ~actual:(Syn.Cst.ModulePath.name field_path);
            Ok ()
        | _ -> Error "expected nested record update");
    Test.case "cst assert expressions preserve the asserted value" (fun () ->
        let source = "let x = assert true\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Assert
                  {
                    asserted =
                      Syn.Cst.Expression.Literal
                        (Syn.Cst.Literal.Bool { literal_token; _ });
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"true"
              ~actual:(Syn.Cst.Token.text literal_token);
            Ok ()
        | _ -> Error "expected assert expression");
    Test.case "cst lazy expressions preserve the wrapped body" (fun () ->
        let source = "let x = lazy (1 + 2)\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.Lazy
                  {
                    body =
                      Syn.Cst.Expression.Parenthesized
                        {
                          inner = Syn.Cst.Expression.Infix _;
                          _;
                        };
                    _;
                  };
              _;
            }
          :: _ ->
            Ok ()
        | _ -> Error "expected lazy expression");
    Test.case "cst while expressions preserve the condition and body"
      (fun () ->
        let source = "let x = while !y < 10 do y := !y + 1 done\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.While
                  {
                    condition = Syn.Cst.Expression.Infix _;
                    body = Syn.Cst.Expression.Assign { operator_token; _ };
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:":="
              ~actual:(Syn.Cst.Token.text operator_token);
            Ok ()
        | _ -> Error "expected while expression");
    Test.case "cst for expressions preserve iterator and direction" (fun () ->
        let source = "let x = for i = 0 downto 1 do f i done\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.For
                  {
                    iterator_token;
                    direction = Syn.Cst.Downto { direction_token };
                    body = Syn.Cst.Expression.Apply _;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"i"
              ~actual:(Syn.Cst.Token.text iterator_token);
            Test.assert_equal ~expected:"downto"
              ~actual:(Syn.Cst.Token.text direction_token);
            Ok ()
        | _ -> Error "expected for expression");
    Test.case "cst for expressions distinguish ascending direction" (fun () ->
        let source = "let x = for i = 0 to 1 do f i done\n" in
        let result = parse_ml source in
        let cst =
          expect_some result.cst
            ~msg:"expected CST for diagnostics-free parse"
          |> Result.expect ~msg:"expected CST for diagnostics-free parse"
        in
        match structure_items cst with
        | Syn.Cst.StructureItem.LetBinding
            {
              value =
                Syn.Cst.Expression.For
                  {
                    direction = Syn.Cst.To { direction_token };
                    body = Syn.Cst.Expression.Apply _;
                    _;
                  };
              _;
            }
          :: _ ->
            Test.assert_equal ~expected:"to"
              ~actual:(Syn.Cst.Token.text direction_token);
            Ok ()
        | _ -> Error "expected ascending for expression");
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"syn-cst" ~tests ~args)
    ~args:Env.args ()
