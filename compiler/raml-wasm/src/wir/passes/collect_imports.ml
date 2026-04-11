open Std
module HashSet = Collections.HashSet
module Types = Types
module Runtime_imports = Runtime_imports

type imports = {
  seen: string HashSet.t;
  ordered_rev: Types.Import.t list;
}

let empty_imports = fun () -> { seen = HashSet.create (); ordered_rev = [] }

let add_import = fun imports import ->
  let key = Types.Import.key import in
  if HashSet.contains imports.seen key then
    imports
  else
    let _ = HashSet.insert imports.seen key in
    { imports with ordered_rev = import :: imports.ordered_rev }

let rec collect_expr = fun imports expr ->
  match expr with
  | Types.Expr.Constant _
  | Types.Expr.Var _ ->
      imports
  | Types.Expr.Direct_call call ->
      let imports =
        match Runtime_imports.import_of_direct_callee call.callee with
        | None -> imports
        | Some import -> add_import imports import
      in
      List.fold_left collect_expr imports call.arguments
  | Types.Expr.Indirect_call call ->
      List.fold_left collect_expr (collect_expr imports call.callee) call.arguments
  | Types.Expr.Lambda lambda ->
      collect_expr imports lambda.body
  | Types.Expr.Let let_ ->
      let imports =
        List.fold_left
          (fun imports (binding: Types.Expr.binding) -> collect_expr imports binding.expr)
          imports
          let_.bindings
      in
      collect_expr imports let_.body
  | Types.Expr.Sequence sequence ->
      collect_expr (collect_expr imports sequence.first) sequence.second
  | Types.Expr.Tuple elements ->
      List.fold_left collect_expr imports elements
  | Types.Expr.Tuple_get tuple_get ->
      collect_expr imports tuple_get.tuple
  | Types.Expr.If_then_else if_then_else ->
      collect_expr
        (collect_expr (collect_expr imports if_then_else.condition) if_then_else.then_)
        if_then_else.else_
  | Types.Expr.Primitive primitive ->
      let imports =
        match Runtime_imports.import_of_primitive_name primitive.name with
        | None -> imports
        | Some import -> add_import imports import
      in
      List.fold_left collect_expr imports primitive.arguments

let collect_global = fun imports (global: Types.Global.t) -> collect_expr imports global.expr

let collect_function = fun imports (function_: Types.Function.t) -> collect_expr imports function_.body

let collect_init_item = fun imports item ->
  match item with
  | Types.Init_item.Global global -> collect_global imports global
  | Types.Init_item.Eval expr -> collect_expr imports expr

let program = fun (program: Types.Compilation_unit.t) ->
  let imports =
    empty_imports ()
    |> fun imports ->
      List.fold_left collect_global imports program.globals |> fun imports ->
        List.fold_left collect_function imports program.functions |> fun imports ->
          List.fold_left collect_init_item imports program.init
  in
  { program with imports = List.rev imports.ordered_rev }
