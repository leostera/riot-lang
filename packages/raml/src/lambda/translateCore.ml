open Std

module TypedTree = Typechecker.TypedTree
module Identifier = Typechecker.Identifier
module ModulePath = Typechecker.ModulePath
module Location = Typechecker.Location

(** {1 TypedTree to Lambda Translation}
    
    Translate type-checked OCaml AST to Lambda IR.
    
    {b For beginners:} This is where we convert from the full OCaml language
    to a simpler intermediate form. Think of it as "lowering" the code:
    - Complex patterns → Simple switches
    - Curried functions → Multi-param functions  
    - Type constructors → Tagged blocks
    - Modules → Flat namespaces
    
    {b Why translate?}
    - Lambda is easier to optimize
    - Lambda is easier to compile to machine code
    - Separates frontend (typing) from backend (code gen)
    
    Example:
    {[
      (* TypedTree: *)
      fun x -> fun y -> x + y
      
      (* Lambda: *)
      Function { params = [x; y]; body = Prim (Pint_add, [Var x; Var y]) }
    ]}
*)

(** {2 Translation Context}
    
    Track information during translation.
*)

type context = {
  mutable next_static_exn : int;
      (** Counter for static exception IDs (used in pattern compilation) *)
}

let create_context () =
  { next_static_exn = 0 }

let fresh_static_exn ctx =
  let id = ctx.next_static_exn in
  ctx.next_static_exn <- ctx.next_static_exn + 1;
  id

(** {2 Constant Translation} *)

let rec translate_constant = function
  | TypedTree.ConstantInt n -> Ir.Const_int n
  | TypedTree.ConstantString s -> Ir.Const_string s
  | TypedTree.ConstantUnit -> Ir.Const_block (0, [])
      (** Unit is represented as a block with tag 0 and no fields *)

(** {2 Pattern Translation (Simplified)}
    
    For now, we only handle simple patterns and compile them to switches.
    Complex patterns will be handled by the pattern match compiler later.
*)

type pattern_translation =
  | Simple of Identifier.t option
      (** Simple pattern: wildcard or variable *)
  | Constructor of int * pattern_translation list
      (** Constructor pattern with tag and sub-patterns *)

let rec translate_pattern_simple pat =
  (** Translate simple patterns (variables, wildcards, constructors).
      
      Returns None for complex patterns that need the full match compiler.
  *)
  match pat.TypedTree.pat_desc with
  | TypedTree.PatternAny ->
      Some (Simple None)
  
  | TypedTree.PatternVar id ->
      Some (Simple (Some id))
  
  | TypedTree.PatternConstant _ ->
      (* Constants need match compiler *)
      None
  
  | TypedTree.PatternTuple _ ->
      (* Tuples need match compiler *)
      None
  
  | TypedTree.PatternConstructor _ ->
      (* Constructors need match compiler *)
      None
  
  | TypedTree.PatternOr _ ->
      (* Or-patterns definitely need match compiler *)
      None
  
  | TypedTree.PatternAlias _ ->
      (* Aliases can be handled but need match compiler *)
      None

(** {2 Expression Translation}
    
    Main translation from TypedTree expressions to Ir.
*)

let rec translate_expression ctx expr =
  (** Translate a typed expression to Lambda IR.
      
      {b Algorithm:}
      - Match on expression type
      - Recursively translate subexpressions
      - Construct appropriate Lambda node
      
      {b Examples:}
      {[
        (* Constant *)
        42 → Const (Const_int 42)
        
        (* Variable *)
        x → Var x_id
        
        (* Let binding *)
        let x = 42 in x + 1 →
        Let { id = x; value = Const 42; body = Prim (Pint_add, [...]) }
      ]}
  *)
  match expr.TypedTree.exp_desc with
  | TypedTree.ExpressionConstant c ->
      Ir.Const (translate_constant c)
  
  | TypedTree.ExpressionIdentifier path ->
      (* For now, only handle simple identifiers *)
      (match path with
       | ModulePath.Identifier id ->
           Ir.Var id
       | _ ->
           (* TODO: Handle module paths *)
           panic "Module paths not yet supported in Lambda translation")
  
  | TypedTree.ExpressionLet { recursive = false; bindings; body } ->
      (* Non-recursive let: translate to nested Llets *)
      translate_let ctx bindings body expr.TypedTree.exp_loc
  
  | TypedTree.ExpressionLet { recursive = true; bindings; body } ->
      (* Recursive let: translate to LetRec *)
      translate_letrec ctx bindings body expr.TypedTree.exp_loc
  
  | TypedTree.ExpressionFunction { param; body } ->
      (* Single-parameter function *)
      Ir.Function {
        params = [param];
        body = translate_expression ctx body;
        loc = expr.TypedTree.exp_loc;
      }
  
  | TypedTree.ExpressionApply { func; arg } ->
      (* Function application *)
      translate_apply ctx func arg expr.TypedTree.exp_loc
  
  | TypedTree.ExpressionMatch { scrutinee; cases } ->
      (* Pattern matching - use simplified version for now *)
      translate_match ctx scrutinee cases expr.TypedTree.exp_loc
  
  | TypedTree.ExpressionIfThenElse { condition; then_branch; else_branch } ->
      (* If/then/else *)
      Ir.IfThenElse (
        translate_expression ctx condition,
        translate_expression ctx then_branch,
        Option.map (translate_expression ctx) else_branch
      )
  
  | TypedTree.ExpressionTuple exprs ->
      (* Tuple: create a block with tag 0 *)
      let fields = List.map (translate_expression ctx) exprs in
      Ir.Prim (Ir.Pmakeblock 0, fields)
  
  | TypedTree.ExpressionConstruct { constructor_path = _; args } ->
      (* Constructor application *)
      (* TODO: Determine tag from constructor *)
      (* For now, use tag 0 *)
      let fields = List.map (translate_expression ctx) args in
      Ir.Prim (Ir.Pmakeblock 0, fields)

and translate_let ctx bindings body loc =
  (** Translate non-recursive let bindings.
      
      Multiple bindings get nested:
      {[
        let x = 1 in let y = 2 in x + y
        →
        Let x = 1 in (Let y = 2 in x + y)
      ]}
  *)
  match bindings with
  | [] ->
      translate_expression ctx body
  | binding :: rest ->
      let value = translate_expression ctx binding.TypedTree.vb_expr in
      let id = match binding.TypedTree.vb_pattern.TypedTree.pat_desc with
        | TypedTree.PatternVar id -> id
        | _ ->
            (* TODO: Handle complex patterns in let bindings *)
            panic "Complex patterns in let bindings not yet supported"
      in
      Ir.Let {
        id;
        value;
        body = translate_let ctx rest body loc;
        loc;
      }

and translate_letrec ctx bindings body loc =
  (** Translate recursive let bindings.
      
      All bindings are mutually recursive:
      {[
        let rec f x = g x
        and g x = f (x - 1)
        in f 10
      ]}
  *)
  let lambda_bindings = bindings
    |> List.map (fun binding ->
        let value = translate_expression ctx binding.TypedTree.vb_expr in
        let id = match binding.TypedTree.vb_pattern.TypedTree.pat_desc with
          | TypedTree.PatternVar id -> id
          | _ -> panic "Complex patterns in letrec not supported"
        in
        (id, value)
      )
  in
  Ir.LetRec {
    bindings = lambda_bindings;
    body = translate_expression ctx body;
    loc;
  }

and translate_apply ctx func arg loc =
  (** Translate function application.
      
      {b Important:} TypedTree has curried application (one arg at a time).
      Lambda has multi-argument application.
      
      We need to collect all nested applications:
      {[
        (* TypedTree: *)
        ((f x) y) z
        
        (* Lambda: *)
        Apply { func = f; args = [x; y; z] }
      ]}
  *)
  let rec collect_args acc expr =
    match expr.TypedTree.exp_desc with
    | TypedTree.ExpressionApply { func; arg } ->
        collect_args (arg :: acc) func
    | _ ->
        (expr, acc)
  in
  
  let (func_expr, all_args) = collect_args [arg] func in
  let lambda_func = translate_expression ctx func_expr in
  let lambda_args = List.map (translate_expression ctx) all_args in
  
  Ir.Apply {
    func = lambda_func;
    args = lambda_args;
    loc;
  }

and translate_match ctx scrutinee cases loc =
  (** Translate pattern matching (simplified version).
      
      {b For now:} Only handle simple variable/wildcard patterns.
      Complex patterns will fail with a helpful error.
      
      {b TODO:} Implement full pattern match compiler.
  *)
  (* For very simple case: just one pattern that's a variable *)
  match cases with
  | [ case ] ->
      (match case.TypedTree.case_pattern.TypedTree.pat_desc with
       | TypedTree.PatternVar id ->
           (* match x with y -> body  ==  let y = x in body *)
           Ir.Let {
             id;
             value = translate_expression ctx scrutinee;
             body = translate_expression ctx case.TypedTree.case_body;
             loc;
           }
       | TypedTree.PatternAny ->
           (* match x with _ -> body  ==  (x; body) *)
           Ir.Sequence (
             translate_expression ctx scrutinee,
             translate_expression ctx case.TypedTree.case_body
           )
       | _ ->
           panic "Complex pattern matching not yet implemented in Lambda translation")
  | _ ->
      panic "Multi-case pattern matching not yet implemented in Lambda translation"

(** {2 Top-Level Translation} *)

let translate_structure_item ctx item =
  (** Translate a structure item (top-level definition). *)
  match item.TypedTree.str_desc with
  | TypedTree.StructureValue { recursive = false; bindings } ->
      (* Top-level let binding *)
      bindings
      |> List.map (fun binding ->
          let value = translate_expression ctx binding.TypedTree.vb_expr in
          let id = match binding.TypedTree.vb_pattern.TypedTree.pat_desc with
            | TypedTree.PatternVar id -> id
            | _ -> panic "Complex patterns in structure items not supported"
          in
          (id, value)
        )
  
  | TypedTree.StructureValue { recursive = true; bindings } ->
      (* Top-level recursive binding *)
      bindings
      |> List.map (fun binding ->
          let value = translate_expression ctx binding.TypedTree.vb_expr in
          let id = match binding.TypedTree.vb_pattern.TypedTree.pat_desc with
            | TypedTree.PatternVar id -> id
            | _ -> panic "Complex patterns in structure items not supported"
          in
          (id, value)
        )
  
  | TypedTree.StructureType _ ->
      (* Type declarations don't generate code *)
      []

let translate_structure structure =
  (** Translate a complete structure (module implementation).
      
      Returns a list of (identifier, lambda) pairs for all top-level bindings.
  *)
  let ctx = create_context () in
  structure
  |> List.map (translate_structure_item ctx)
  |> List.flatten
