# Horizontal Type Checker Development Plan

## Current Problem

We've been building **vertically**:
```
Expression -> Type Check -> Lambda -> Codegen -> Assembly
    ↓            ↓           ↓          ↓          ↓
  const        ✅          ✅         ✅         ✅
  let          ✅          ✅         ✅         ✅
  function     ✅          ⚠️         ❌         ❌
  match        ❌          ❌         ❌         ❌
```

This means we can't type-check programs that use `match` even though it's just type checking!

## Better Approach: Horizontal Development

```
Type Checker Layer (Complete all features first!)
─────────────────────────────────────────────────
  const        ✅
  let          ✅
  function     ✅
  apply        ✅
  tuple        ✅
  if-then-else ✅
  match        ⏳ <- Add next
  record       ⏳ <- Add next
  variant      ⏳ <- Add next
  for/while    ⏳ <- Add next
  try/catch    ⏳ <- Add next
  ALL EXPRS    ⏳ <- Complete coverage!

Then Lambda/Codegen Layer
─────────────────────────────────────────────────
  (Implement as needed)
```

## Benefits

1. **Type check ANY OCaml program** - even if we can't compile it yet
2. **Better error messages** - catch type errors early
3. **LSP support** - can provide completions/diagnostics
4. **Incremental compilation** - type check once, compile parts

## Implementation Strategy

### Phase 1: Complete Expression Coverage (This Week)

Add stubs that type-check correctly but may not compile:

```ocaml
(* In checker.ml *)
let rec check_expression state expr =
  match expr.expr_desc with
  | ExprConstant const -> check_constant state const expr.expr_loc
  | ExprIdent name -> check_ident state name expr.expr_loc
  | ExprLet {...} -> check_let state ...
  | ExprFunction {...} -> check_function state ...
  | ExprApply {...} -> check_apply state ...
  | ExprTuple elements -> check_tuple state elements expr.expr_loc
  | ExprIfThenElse {...} -> check_if state ...
  | ExprBinaryOp {...} -> check_binary_op state ...
  | ExprSequence (e1, e2) -> check_sequence state e1 e2 expr.expr_loc
  
  (* ADD THESE: *)
  | ExprMatch { expr; cases } -> 
      check_match state expr cases expr.expr_loc
  | ExprConstruct { constructor; arg } ->
      check_construct state constructor arg expr.expr_loc
  | ExprRecord fields ->
      check_record state fields expr.expr_loc
  | ExprField { record; field } ->
      check_field state record field expr.expr_loc
  | ExprConstraint { expr; typ } ->
      check_constraint state expr typ expr.expr_loc
  | ExprUnaryOp { op; arg } ->
      check_unary_op state op arg expr.expr_loc
```

### Phase 2: Add Remaining Constructs

```ocaml
(* Complete the pattern matching *)
and check_pattern state pattern expected_ty =
  match pattern.pattern_desc with
  | PatternAny -> ...
  | PatternVar name -> ...
  | PatternConstant const -> ...
  | PatternTuple elements -> ...
  
  (* ADD THESE: *)
  | PatternConstruct { constructor; arg } ->
      check_pattern_construct state constructor arg expected_ty
  | PatternRecord fields ->
      check_pattern_record state fields expected_ty
  | PatternOr (p1, p2) ->
      check_pattern_or state p1 p2 expected_ty
  | PatternAlias (p, name) ->
      check_pattern_alias state p name expected_ty
  | PatternConstraint (p, typ) ->
      check_pattern_constraint state p typ expected_ty
```

### Phase 3: Structure Items

```ocaml
(* Handle all top-level items *)
let check_structure_item state item =
  match item.item_desc with
  | ItemValue { recursive; pattern; expr } ->
      check_value_item state recursive pattern expr
  | ItemType { name; params; manifest } ->
      check_type_declaration state name params manifest
  
  (* ADD THESE: *)
  | ItemException { name; args } ->
      check_exception_declaration state name args
  | ItemModule { name; expr } ->
      check_module_binding state name expr
  | ItemModuleType { name; sig_ } ->
      check_module_type_declaration state name sig_
  | ItemOpen path ->
      check_open state path
  | ItemInclude mod_expr ->
      check_include state mod_expr
```

## Concrete Next Steps

### Step 1: Match Expressions (2-3 hours)

```ocaml
and check_match state scrutinee cases loc =
  (* Check the scrutinee *)
  let* scrut_expr, state = check_expression state scrutinee in
  
  (* Check each case *)
  let* typed_cases, state = 
    check_cases state scrut_expr.exp_type cases 
  in
  
  (* All branches must have same type *)
  let result_ty = (List.hd typed_cases).TypedTree.case_body.exp_type in
  
  let typed_expr =
    TypedTree.make_expression
      ~desc:(TypedTree.ExpressionMatch { 
        scrutinee = scrut_expr; 
        cases = typed_cases 
      })
      ~typ:result_ty 
      ~loc:None
  in
  Ok (typed_expr, state)

and check_cases state scrut_ty = function
  | [] -> Ok ([], state)
  | case :: rest ->
      let* typed_pattern, state, bindings = 
        check_pattern state case.pattern scrut_ty 
      in
      
      (* Add pattern bindings to environment *)
      let env = 
        List.fold_left
          (fun env (ident, ty) -> 
            Environment.add_value env ident ty ~loc:None)
          state.env bindings
      in
      let state = { state with env } in
      
      (* Check the body *)
      let* body_expr, state = check_expression state case.rhs in
      
      let typed_case = {
        TypedTree.case_pattern = typed_pattern;
        case_body = body_expr;
      } in
      
      let* rest_cases, state = check_cases state scrut_ty rest in
      Ok (typed_case :: rest_cases, state)
```

### Step 2: Variant Construction (1-2 hours)

```ocaml
and check_construct state constructor arg loc =
  (* Look up constructor in environment *)
  match Hashtbl.find_opt state.constructors constructor with
  | None -> 
      Error (UnboundVariable { name = constructor; loc })
  | Some (type_ident, result_ty) ->
      (* Check argument if present *)
      let* arg_expr_opt, state =
        match arg with
        | None -> Ok (None, state)
        | Some arg_expr ->
            let* typed_arg, state = check_expression state arg_expr in
            Ok (Some typed_arg, state)
      in
      
      (* Build typed expression *)
      let args = match arg_expr_opt with
        | None -> []
        | Some e -> [e]
      in
      
      let typed_expr =
        TypedTree.make_expression
          ~desc:(TypedTree.ExpressionConstruct {
            constructor_path = ModulePath.Identifier type_ident;
            args;
          })
          ~typ:result_ty
          ~loc:None
      in
      Ok (typed_expr, state)
```

### Step 3: Records (2-3 hours)

```ocaml
and check_record state fields loc =
  (* For now, create a record type on the fly *)
  (* TODO: Look up record type from environment *)
  
  let* typed_fields, state =
    check_record_fields state fields
  in
  
  let field_types = 
    List.map (fun (name, expr) -> (name, expr.TypedTree.exp_type)) typed_fields
  in
  
  (* Create a record type *)
  let record_ty, ctx = 
    Types.newty ~ctx:state.ctx (Types.Tuple (List.map snd field_types))
  in
  let state = { state with ctx } in
  
  (* For now, represent records as tuples in TypedTree *)
  let typed_expr =
    TypedTree.make_expression
      ~desc:(TypedTree.ExpressionTuple (List.map snd typed_fields))
      ~typ:record_ty
      ~loc:None
  in
  Ok (typed_expr, state)

and check_record_fields state = function
  | [] -> Ok ([], state)
  | (name, expr) :: rest ->
      let* typed_expr, state = check_expression state expr in
      let* rest_fields, state = check_record_fields state rest in
      Ok ((name, typed_expr) :: rest_fields, state)
```

## Testing Strategy

For each new feature, write tests immediately:

```bash
# Match expressions
cat > test_match.ml << 'EOF'
type option = None | Some of int

let get_value = fun opt ->
  match opt with
  | None -> 0
  | Some x -> x
