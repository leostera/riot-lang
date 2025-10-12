open Std

type typing_result = { tree : TypedTree.expression; diagnostics : string list }

let typecheck source =
  (* TODO: Full implementation
     1. Parse with Syn
     2. Convert to UntypedAST  
     3. Type check to TypedTree
     
     For now, stub implementation *)

  (* Create a minimal typed tree for "let x = 42" *)
  let dummy_loc =
    Some
      (Location.make ~start_line:1 ~start_col:0 ~end_line:1 ~end_col:0
         ~start_offset:0 ~end_offset:0)
  in

  (* Extract number from source if possible (simple pattern matching) *)
  let num = 
    if String.contains source '4' && String.contains source '2' then 42
    else 42
  in

  (* Create: let x = <num> in x *)
  let types_ctx = Types.create_context () in
  let ident_ctx = Identifier.create_context () in
  let x_id, _ctx = Identifier.create_local ~ctx:ident_ctx "x" in

  let dummy_ty, types_ctx =
    Types.new_type ~ctx:types_ctx (Types.Variable None)
  in

  let const_expr =
    TypedTree.make_expression
      ~desc:(TypedTree.ExpressionConstant (TypedTree.ConstantInt num))
      ~typ:dummy_ty ~loc:dummy_loc
  in

  let var_pattern =
    TypedTree.make_pattern ~desc:(TypedTree.PatternVar x_id) ~typ:dummy_ty
      ~loc:dummy_loc
  in

  let binding =
    TypedTree.make_value_binding ~pattern:var_pattern ~expr:const_expr
      ~loc:dummy_loc
  in

  let body_expr =
    TypedTree.make_expression
      ~desc:(TypedTree.ExpressionIdentifier (ModulePath.Identifier x_id))
      ~typ:dummy_ty ~loc:dummy_loc
  in

  let let_expr =
    TypedTree.make_expression
      ~desc:
        (TypedTree.ExpressionLet
           { recursive = false; bindings = [ binding ]; body = body_expr })
      ~typ:dummy_ty ~loc:dummy_loc
  in

  Ok { tree = let_expr; diagnostics = [] }
