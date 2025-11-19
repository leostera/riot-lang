open Std

(* Simple format helper - handles basic %s substitution *)
let format template = 
  let rec replace str args = 
    match args with 
    | [] -> str 
    | arg :: rest -> 
        let idx = try String.index str '%' with Not_found -> -1 in 
        if idx = -1 then str ^ arg 
        else 
          let before = String.sub str 0 idx in 
          let after_idx = idx + 2 in 
          let after = if after_idx < String.length str 
                     then String.sub str after_idx (String.length str - after_idx) 
                     else "" in 
          replace (before ^ arg ^ after) rest 
  in replace template

let ( let* ) = Result.and_then

type typing_result = { tree : TypedTree.expression; diagnostics : string list }

type type_error =
  | ParseError of string
  | ConversionError of FromSyntax.error list
  | UnboundVariable of { name : string; loc : UntypedTree.location }
  | TypeMismatch of {
      expected : Types.type_expr;
      got : Types.type_expr;
      loc : UntypedTree.location;
    }
  | OccursCheck of { var : Types.type_expr; typ : Types.type_expr }
  | NotImplemented of { feature : string; loc : UntypedTree.location }

type checker_state = {
  env : Environment.t;
  ctx : Types.context;
  ident_ctx : Identifier.context;
  name_map : (string, Identifier.t) Collections.HashMap.t;
  constructors : (string, Identifier.t * Types.type_expr) Collections.HashMap.t;
}

let make_state () =
  let ctx = Types.create_context () in
  let env = Environment.create () in
  let env, ctx = Environment.add_predef_types env ~ctx in
  let ident_ctx = Identifier.create_context () in
  let name_map = Collections.HashMap.create () in
  let constructors = Collections.HashMap.create () in
  { env; ctx; ident_ctx; name_map; constructors }

let rec check_expression state (expr : UntypedTree.expression) =
  match expr.expr_desc with
  | ExprConstant const -> check_constant state const expr.expr_loc
  | ExprIdent name -> check_ident state name expr.expr_loc
  | ExprLet { recursive; pattern; value; body } ->
      check_let state recursive pattern value body expr.expr_loc
  | ExprFunction { param; body } ->
      check_function state param body expr.expr_loc
  | ExprApply { func; arg } -> check_apply state func arg expr.expr_loc
  | ExprTuple elements -> check_tuple state elements expr.expr_loc
  | ExprIfThenElse { condition; then_branch; else_branch } ->
      check_if state condition then_branch else_branch expr.expr_loc
  | ExprBinaryOp { op; left; right } ->
      check_binary_op state op left right expr.expr_loc
  | ExprSequence (e1, e2) -> check_sequence state e1 e2 expr.expr_loc
  | ExprMatch { expr = scrutinee; cases } ->
      check_match state scrutinee cases expr.expr_loc
  | ExprConstruct { constructor; arg } ->
      check_construct state constructor arg expr.expr_loc
  | ExprRecord fields -> check_record state fields expr.expr_loc
  | ExprField { record; field } -> check_field state record field expr.expr_loc
  | ExprConstraint { expr = e; typ } ->
      check_constraint state e typ expr.expr_loc
  | ExprUnaryOp { op; arg } -> check_unary_op state op arg expr.expr_loc

and check_constant state const loc =
  match const with
  | UntypedTree.ConstantInt n ->
      let ty, ctx = Environment.type_int ~ctx:state.ctx in
      let state = { state with ctx } in
      let typed_expr =
        TypedTree.make_expression
          ~desc:(TypedTree.ExpressionConstant (TypedTree.ConstantInt n)) ~typ:ty
          ~loc:None
      in
      Ok (typed_expr, state)
  | UntypedTree.ConstantString s ->
      let ty, ctx = Environment.type_string ~ctx:state.ctx in
      let state = { state with ctx } in
      let typed_expr =
        TypedTree.make_expression
          ~desc:(TypedTree.ExpressionConstant (TypedTree.ConstantString s))
          ~typ:ty ~loc:None
      in
      Ok (typed_expr, state)
  | UntypedTree.ConstantUnit ->
      let ty, ctx = Environment.type_unit ~ctx:state.ctx in
      let state = { state with ctx } in
      let typed_expr =
        TypedTree.make_expression
          ~desc:(TypedTree.ExpressionConstant TypedTree.ConstantUnit) ~typ:ty
          ~loc:None
      in
      Ok (typed_expr, state)
  | ConstantFloat _ ->
      Error (NotImplemented { feature = "float constants"; loc })
  | ConstantChar _ -> Error (NotImplemented { feature = "char constants"; loc })
  | ConstantBool _ -> Error (NotImplemented { feature = "bool constants"; loc })

and check_ident state name loc =
  match Collections.HashMap.get state.name_map name with
  | Some ident -> (
      match Environment.find_value_type state.env ident with
      | Some ty ->
          let typed_expr =
            TypedTree.make_expression
              ~desc:
                (TypedTree.ExpressionIdentifier (ModulePath.Identifier ident))
              ~typ:ty ~loc:None
          in
          Ok (typed_expr, state)
      | None -> Error (UnboundVariable { name; loc }))
  | None -> Error (UnboundVariable { name; loc })

and check_let state recursive pattern value body loc =
  let* value_expr, state = check_expression state value in
  let* pattern, state, bindings =
    check_pattern state pattern value_expr.exp_type
  in
  let env =
    List.fold_left
      (fun env (ident, ty) -> Environment.add_value env ident ty ~loc:None)
      state.env bindings
  in
  let state = { state with env } in
  let* body_expr, state = check_expression state body in
  let binding =
    TypedTree.make_value_binding ~pattern ~expr:value_expr ~loc:None
  in
  let typed_expr =
    TypedTree.make_expression
      ~desc:
        (TypedTree.ExpressionLet
           { recursive; bindings = [ binding ]; body = body_expr })
      ~typ:body_expr.exp_type ~loc:None
  in
  Ok (typed_expr, state)

and check_pattern state pattern expected_ty =
  match pattern.UntypedTree.pattern_desc with
  | PatternVar name ->
      let ident, ident_ctx =
        Identifier.create_local ~ctx:state.ident_ctx name
      in
      let _ = Collections.HashMap.insert state.name_map name ident in
      let state = { state with ident_ctx } in
      let typed_pattern =
        TypedTree.make_pattern ~desc:(TypedTree.PatternVar ident)
          ~typ:expected_ty ~loc:None
      in
      Ok (typed_pattern, state, [ (ident, expected_ty) ])
  | PatternAny ->
      let typed_pattern =
        TypedTree.make_pattern ~desc:TypedTree.PatternAny ~typ:expected_ty
          ~loc:None
      in
      Ok (typed_pattern, state, [])
  | PatternConstant const -> (
      match const with
      | ConstantInt n ->
          let const_ty, ctx = Environment.type_int ~ctx:state.ctx in
          let state = { state with ctx } in
          let typed_pattern =
            TypedTree.make_pattern
              ~desc:(TypedTree.PatternConstant (TypedTree.ConstantInt n))
              ~typ:const_ty ~loc:None
          in
          Ok (typed_pattern, state, [])
      | ConstantString s ->
          let const_ty, ctx = Environment.type_string ~ctx:state.ctx in
          let state = { state with ctx } in
          let typed_pattern =
            TypedTree.make_pattern
              ~desc:(TypedTree.PatternConstant (TypedTree.ConstantString s))
              ~typ:const_ty ~loc:None
          in
          Ok (typed_pattern, state, [])
      | ConstantUnit ->
          let const_ty, ctx = Environment.type_unit ~ctx:state.ctx in
          let state = { state with ctx } in
          let typed_pattern =
            TypedTree.make_pattern
              ~desc:(TypedTree.PatternConstant TypedTree.ConstantUnit)
              ~typ:const_ty ~loc:None
          in
          Ok (typed_pattern, state, [])
      | _ ->
          Error
            (NotImplemented
               {
                 feature = "unsupported pattern constant";
                 loc = pattern.pattern_loc;
               }))
  | PatternTuple elements ->
      let* typed_patterns, state, all_bindings =
        check_pattern_tuple state elements expected_ty
      in
      let typed_pattern =
        TypedTree.make_pattern ~desc:(TypedTree.PatternTuple typed_patterns)
          ~typ:expected_ty ~loc:None
      in
      Ok (typed_pattern, state, all_bindings)
  | PatternConstruct { constructor; arg } ->
      check_pattern_construct state constructor arg expected_ty
        pattern.pattern_loc
  | PatternOr (p1, p2) ->
      let* typed_p1, state1, bindings1 = check_pattern state p1 expected_ty in
      let* typed_p2, state2, bindings2 = check_pattern state p2 expected_ty in
      let typed_pattern =
        TypedTree.make_pattern
          ~desc:(TypedTree.PatternOr (typed_p1, typed_p2))
          ~typ:expected_ty ~loc:None
      in
      Ok (typed_pattern, state2, bindings1 @ bindings2)
  | PatternAlias (p, name) ->
      let* typed_p, state, bindings = check_pattern state p expected_ty in
      let ident, ident_ctx =
        Identifier.create_local ~ctx:state.ident_ctx name
      in
      let _ = Collections.HashMap.insert state.name_map name ident in
      let state = { state with ident_ctx } in
      let typed_pattern =
        TypedTree.make_pattern
          ~desc:(TypedTree.PatternAlias (typed_p, ident))
          ~typ:expected_ty ~loc:None
      in
      Ok (typed_pattern, state, (ident, expected_ty) :: bindings)
  | PatternConstraint (p, _typ) -> check_pattern state p expected_ty
  | PatternRecord _fields ->
      Error
        (NotImplemented
           { feature = "record patterns"; loc = pattern.pattern_loc })

and check_pattern_tuple state elements expected_ty =
  match elements with
  | [] -> Ok ([], state, [])
  | elem :: rest ->
      let elem_ty, ctx = Types.newvar ~ctx:state.ctx None in
      let state = { state with ctx } in
      let* typed_elem, state, bindings = check_pattern state elem elem_ty in
      let* typed_rest, state, rest_bindings =
        check_pattern_tuple state rest expected_ty
      in
      Ok (typed_elem :: typed_rest, state, bindings @ rest_bindings)

and check_pattern_construct state constructor arg expected_ty loc =
  match Collections.HashMap.get state.constructors constructor with
  | None -> Error (UnboundVariable { name = constructor; loc })
  | Some (type_ident, result_ty) ->
      let* typed_args, state, bindings =
        match arg with
        | None -> Ok ([], state, [])
        | Some arg_pattern ->
            let arg_ty, ctx = Types.newvar ~ctx:state.ctx None in
            let state = { state with ctx } in
            let* typed_arg, state, bindings =
              check_pattern state arg_pattern arg_ty
            in
            Ok ([ typed_arg ], state, bindings)
      in
      let typed_pattern =
        TypedTree.make_pattern
          ~desc:
            (TypedTree.PatternConstructor
               {
                 constructor_path = ModulePath.Identifier type_ident;
                 args = typed_args;
               })
          ~typ:result_ty ~loc:None
      in
      Ok (typed_pattern, state, bindings)

and check_tuple state elements loc =
  let rec check_elements acc state = function
    | [] -> Ok (List.rev acc, state)
    | elem :: rest ->
        let* typed_elem, state = check_expression state elem in
        check_elements (typed_elem :: acc) state rest
  in
  let* typed_elements, state = check_elements [] state elements in
  let element_types = List.map (fun e -> e.TypedTree.exp_type) typed_elements in
  let tuple_ty, ctx = Types.newty ~ctx:state.ctx (Types.Tuple element_types) in
  let state = { state with ctx } in
  let typed_expr =
    TypedTree.make_expression ~desc:(TypedTree.ExpressionTuple typed_elements)
      ~typ:tuple_ty ~loc:None
  in
  Ok (typed_expr, state)

and check_if state condition then_branch else_branch loc =
  let* cond_expr, state = check_expression state condition in
  let bool_ty, ctx = Environment.type_bool ~ctx:state.ctx in
  let state = { state with ctx } in
  let* then_expr, state = check_expression state then_branch in
  match else_branch with
  | Some else_branch ->
      let* else_expr, state = check_expression state else_branch in
      let typed_expr =
        TypedTree.make_expression
          ~desc:
            (TypedTree.ExpressionIfThenElse
               {
                 condition = cond_expr;
                 then_branch = then_expr;
                 else_branch = Some else_expr;
               })
          ~typ:then_expr.exp_type ~loc:None
      in
      Ok (typed_expr, state)
  | None ->
      let typed_expr =
        TypedTree.make_expression
          ~desc:
            (TypedTree.ExpressionIfThenElse
               {
                 condition = cond_expr;
                 then_branch = then_expr;
                 else_branch = None;
               })
          ~typ:then_expr.exp_type ~loc:None
      in
      Ok (typed_expr, state)

and check_function state param body loc =
  let param_ty, ctx = Types.newvar ~ctx:state.ctx None in
  let state = { state with ctx } in
  let* typed_param, state, bindings = check_pattern state param param_ty in
  let param_ident =
    match typed_param.TypedTree.pat_desc with
    | PatternVar id -> id
    | _ ->
        let id, ident_ctx =
          Identifier.create_local ~ctx:state.ident_ctx "_param"
        in
        id
  in
  let env =
    List.fold_left
      (fun env (ident, ty) -> Environment.add_value env ident ty ~loc:None)
      state.env bindings
  in
  let state = { state with env } in
  let* body_expr, state = check_expression state body in
  let fn_ty, ctx =
    Environment.type_arrow ~ctx:state.ctx Types.Nolabel param_ty
      body_expr.exp_type
  in
  let state = { state with ctx } in
  let typed_expr =
    TypedTree.make_expression
      ~desc:
        (TypedTree.ExpressionFunction { param = param_ident; body = body_expr })
      ~typ:fn_ty ~loc:None
  in
  Ok (typed_expr, state)

and check_apply state func arg loc =
  let* func_expr, state = check_expression state func in
  let* arg_expr, state = check_expression state arg in
  let result_ty, ctx = Types.newvar ~ctx:state.ctx None in
  let state = { state with ctx } in
  let expected_fn_ty, ctx =
    Environment.type_arrow ~ctx:state.ctx Types.Nolabel arg_expr.exp_type
      result_ty
  in
  let state = { state with ctx } in
  let* ctx =
    match
      Unification.unify ~ctx:state.ctx func_expr.exp_type expected_fn_ty
    with
    | Ok ctx -> Ok ctx
    | Error _ ->
        Error
          (TypeMismatch
             { expected = expected_fn_ty; got = func_expr.exp_type; loc })
  in
  let state = { state with ctx } in
  let typed_expr =
    TypedTree.make_expression
      ~desc:(TypedTree.ExpressionApply { func = func_expr; arg = arg_expr })
      ~typ:result_ty ~loc:None
  in
  Ok (typed_expr, state)

and check_binary_op state op left right loc =
  let* left_expr, state = check_expression state left in
  let* right_expr, state = check_expression state right in
  let result_ty =
    match op with
    | UntypedTree.Add | Sub | Mul | Div | Mod -> left_expr.exp_type
    | Eq | Neq | Lt | Le | Gt | Ge | And | Or ->
        let ty, ctx = Environment.type_bool ~ctx:state.ctx in
        let state = { state with ctx } in
        ty
    | Cons | At -> left_expr.exp_type
  in
  let typed_expr =
    TypedTree.make_expression
      ~desc:
        (TypedTree.ExpressionApply
           {
             func =
               TypedTree.make_expression
                 ~desc:
                   (TypedTree.ExpressionApply
                      {
                        func =
                          TypedTree.make_expression
                            ~desc:
                              (TypedTree.ExpressionIdentifier
                                 (ModulePath.Identifier
                                    (fst
                                       (Identifier.create_local
                                          ~ctx:state.ident_ctx "_binop"))))
                            ~typ:result_ty ~loc:None;
                        arg = left_expr;
                      })
                 ~typ:result_ty ~loc:None;
             arg = right_expr;
           })
      ~typ:result_ty ~loc:None
  in
  Ok (typed_expr, state)

and check_sequence state e1 e2 loc =
  let* expr1, state = check_expression state e1 in
  let* expr2, state = check_expression state e2 in
  let typed_expr =
    TypedTree.make_expression
      ~desc:
        (TypedTree.ExpressionApply
           {
             func =
               TypedTree.make_expression
                 ~desc:
                   (TypedTree.ExpressionApply
                      {
                        func =
                          TypedTree.make_expression
                            ~desc:
                              (TypedTree.ExpressionIdentifier
                                 (ModulePath.Identifier
                                    (fst
                                       (Identifier.create_local
                                          ~ctx:state.ident_ctx "_seq"))))
                            ~typ:expr2.exp_type ~loc:None;
                        arg = expr1;
                      })
                 ~typ:expr2.exp_type ~loc:None;
             arg = expr2;
           })
      ~typ:expr2.exp_type ~loc:None
  in
  Ok (typed_expr, state)

and check_match state scrutinee cases loc =
  let* scrut_expr, state = check_expression state scrutinee in
  let* typed_cases, state = check_cases state scrut_expr.exp_type cases in

  let result_ty =
    match typed_cases with
    | [] ->
        let ty, ctx = Types.newvar ~ctx:state.ctx None in
        let state = { state with ctx } in
        ty
    | case :: _ -> case.TypedTree.case_body.exp_type
  in

  let typed_expr =
    TypedTree.make_expression
      ~desc:
        (TypedTree.ExpressionMatch
           { scrutinee = scrut_expr; cases = typed_cases })
      ~typ:result_ty ~loc:None
  in
  Ok (typed_expr, state)

and check_cases state scrut_ty = function
  | [] -> Ok ([], state)
  | case :: rest ->
      let* typed_pattern, state, bindings =
        check_pattern state case.UntypedTree.pattern scrut_ty
      in
      let env =
        List.fold_left
          (fun env (ident, ty) -> Environment.add_value env ident ty ~loc:None)
          state.env bindings
      in
      let state = { state with env } in
      let* body_expr, state = check_expression state case.rhs in
      let typed_case =
        { TypedTree.case_pattern = typed_pattern; case_body = body_expr }
      in
      let* rest_cases, state = check_cases state scrut_ty rest in
      Ok (typed_case :: rest_cases, state)

and check_construct state constructor arg loc =
  match Collections.HashMap.get state.constructors constructor with
  | None -> Error (UnboundVariable { name = constructor; loc })
  | Some (type_ident, result_ty) ->
      let* args, state =
        match arg with
        | None -> Ok ([], state)
        | Some arg_expr ->
            let* typed_arg, state = check_expression state arg_expr in
            Ok ([ typed_arg ], state)
      in
      let typed_expr =
        TypedTree.make_expression
          ~desc:
            (TypedTree.ExpressionConstruct
               { constructor_path = ModulePath.Identifier type_ident; args })
          ~typ:result_ty ~loc:None
      in
      Ok (typed_expr, state)

and check_record state fields loc =
  let* typed_fields, state = check_record_fields state fields in
  let field_types =
    List.map (fun (_name, expr) -> expr.TypedTree.exp_type) typed_fields
  in
  let record_ty, ctx = Types.newty ~ctx:state.ctx (Types.Tuple field_types) in
  let state = { state with ctx } in
  let typed_expr =
    TypedTree.make_expression
      ~desc:(TypedTree.ExpressionTuple (List.map snd typed_fields))
      ~typ:record_ty ~loc:None
  in
  Ok (typed_expr, state)

and check_record_fields state = function
  | [] -> Ok ([], state)
  | (name, expr) :: rest ->
      let* typed_expr, state = check_expression state expr in
      let* rest_fields, state = check_record_fields state rest in
      Ok ((name, typed_expr) :: rest_fields, state)

and check_field state record field loc =
  let* record_expr, state = check_expression state record in
  let field_ty, ctx = Types.newvar ~ctx:state.ctx None in
  let state = { state with ctx } in
  let typed_expr =
    TypedTree.make_expression ~desc:(TypedTree.ExpressionTuple [ record_expr ])
      ~typ:field_ty ~loc:None
  in
  Ok (typed_expr, state)

and check_constraint state expr typ loc =
  let* typed_expr, state = check_expression state expr in
  Ok (typed_expr, state)

and check_unary_op state op arg loc =
  let* arg_expr, state = check_expression state arg in
  let result_ty = arg_expr.exp_type in
  let typed_expr =
    TypedTree.make_expression
      ~desc:
        (TypedTree.ExpressionApply
           {
             func =
               TypedTree.make_expression
                 ~desc:
                   (TypedTree.ExpressionIdentifier
                      (ModulePath.Identifier
                         (fst
                            (Identifier.create_local ~ctx:state.ident_ctx
                               "_unop"))))
                 ~typ:result_ty ~loc:None;
             arg = arg_expr;
           })
      ~typ:result_ty ~loc:None
  in
  Ok (typed_expr, state)

let check_type_declaration state (item : UntypedTree.structure_item) =
  match item.item_desc with
  | ItemType { name; params; manifest } ->
      let type_ident, ident_ctx =
        Identifier.create_local ~ctx:state.ident_ctx name
      in
      let state = { state with ident_ctx } in

      let param_types, ctx =
        List.fold_left
          (fun (acc, ctx) param_name ->
            let param_ty, ctx = Types.newvar ~ctx (Some param_name) in
            (param_ty :: acc, ctx))
          ([], state.ctx) params
      in
      let param_types = List.rev param_types in
      let state = { state with ctx } in

      let type_decl, state =
        match manifest with
        | UntypedTree.TypeVariant constructors ->
            let constructor_decls, state =
              List.fold_left
                (fun (acc, state) cstr ->
                  let cstr_ident, ident_ctx =
                    Identifier.create_local ~ctx:state.ident_ctx
                      cstr.UntypedTree.constructor_name
                  in
                  let state = { state with ident_ctx } in

                  let cstr_args, ctx =
                    match cstr.constructor_arg with
                    | Some _arg_ty ->
                        let arg_ty, ctx = Types.newvar ~ctx:state.ctx None in
                        (Types.ConstructorTuple [ arg_ty ], ctx)
                    | None -> (Types.ConstructorTuple [], state.ctx)
                  in
                  let state = { state with ctx } in

                  let result_ty, ctx =
                    Types.newty ~ctx:state.ctx
                      (Types.Constructor
                         (ModulePath.Identifier type_ident, param_types))
                  in
                  let state = { state with ctx } in

                  let _ = Collections.HashMap.insert state.constructors cstr.constructor_name
                    (type_ident, result_ty) in

                  let cstr_decl =
                    {
                      Types.cd_name = cstr.constructor_name;
                      cd_args = cstr_args;
                      cd_res = Some result_ty;
                    }
                  in
                  (cstr_decl :: acc, state))
                ([], state) constructors
            in
            let constructor_decls = List.rev constructor_decls in

            let type_decl =
              {
                Types.type_params = param_types;
                type_arity = List.length params;
                type_kind = Types.Variant constructor_decls;
                type_manifest = None;
                type_variance = [];
              }
            in
            (type_decl, state)
        | UntypedTree.TypeRecord fields ->
            let field_decls =
              List.map
                (fun fld ->
                  {
                    Types.ld_name = fld.UntypedTree.field_name;
                    ld_mutable = fld.field_mutable;
                    ld_type = fst (Types.newvar ~ctx:state.ctx None);
                  })
                fields
            in

            let type_decl =
              {
                Types.type_params = param_types;
                type_arity = List.length params;
                type_kind = Types.Record field_decls;
                type_manifest = None;
                type_variance = [];
              }
            in
            (type_decl, state)
        | UntypedTree.TypeAlias _core_ty ->
            let alias_ty, ctx = Types.newvar ~ctx:state.ctx None in
            let state = { state with ctx } in
            let type_decl =
              {
                Types.type_params = param_types;
                type_arity = List.length params;
                type_kind = Types.Abstract;
                type_manifest = Some alias_ty;
                type_variance = [];
              }
            in
            (type_decl, state)
      in

      let _ = Collections.HashMap.insert state.name_map name type_ident in
      let env = Environment.add_type state.env type_ident type_decl ~loc:None in
      let state = { state with env } in

      Ok state
  | _ -> Error (ParseError "Expected type declaration")

let error_to_string err =
  match err with
  | ParseError s -> "Parse error: " ^ s
  | ConversionError _ -> "Conversion error"
  | UnboundVariable { name; _ } -> "Unbound variable: " ^ name
  | TypeMismatch _ -> "Type mismatch"
  | OccursCheck _ -> "Occurs check"
  | NotImplemented { feature; _ } -> "Not implemented: " ^ feature

let typecheck source =
  let tokens = Syn.Lexer.tokenize source in
  let parse_result = Syn.Parser.parse_implementation ~source tokens in
  match parse_result.diagnostics with
  | _ :: _ -> Error "Parse errors found"
  | [] -> (
      match FromSyntax.from_parse_result parse_result with
      | Error _errors -> Error "Conversion error"
      | Ok structure -> (
          let state = make_state () in

          let rec process_structure state items =
            match items with
            | [] -> Ok (state, None)
            | item :: rest -> (
                match item.UntypedTree.item_desc with
                | ItemType _ ->
                    let* state = check_type_declaration state item in
                    process_structure state rest
                | ItemValue { pattern; expr; _ } ->
                    let* typed_expr, state = check_expression state expr in
                    Ok (state, Some typed_expr))
          in

          match process_structure state structure with
          | Ok (_state, Some typed_expr) ->
              Ok { tree = typed_expr; diagnostics = [] }
          | Ok (_state, None) -> Error "No expression to check"
          | Error err -> Error (error_to_string err)))
