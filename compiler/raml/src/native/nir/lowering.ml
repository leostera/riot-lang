open Std
open Std.Data
module Core = Core_ir
module Nir = Types

type error =
  | UnsupportedModuleKind of { kind: Source_unit.kind }
  | UnsupportedGroup of { group_index: int; reason: string }
  | UnsupportedBinding of { name: string; reason: string }
  | UnsupportedExpr of { reason: string }

type 'value validation = ('value, error list) result

type lowered_item =
  | LoweredFunction of Nir.Function.t
  | LoweredEntry of Nir.Entry_item.t

let ok = fun value -> Ok value

let error = fun value -> Error [ value ]

let validation_map2 = fun left right f ->
  match (left, right) with
  | (Ok left, Ok right) -> Ok (f left right)
  | (Error left, Ok _) -> Error left
  | (Ok _, Error right) -> Error right
  | (Error left, Error right) -> Error (left @ right)

let map_results = fun items f ->
  List.fold_right
    (fun item acc -> validation_map2 (f item) acc (fun item acc -> item :: acc))
    items
    (Ok [])

let source_kind_to_string = fun kind ->
  match kind with
  | Source_unit.Implementation -> "implementation"
  | Source_unit.Interface -> "interface"

let error_to_json = fun error ->
  match error with
  | UnsupportedModuleKind { kind } -> Json.obj
    [
      ("kind", Json.string "unsupported_module_kind");
      ("source_kind", Json.string (source_kind_to_string kind));
    ]
  | UnsupportedGroup { group_index; reason } -> Json.obj
    [
      ("group_index", Json.int group_index);
      ("kind", Json.string "unsupported_group");
      ("reason", Json.string reason);
    ]
  | UnsupportedBinding { name; reason } -> Json.obj
    [
      ("kind", Json.string "unsupported_binding");
      ("name", Json.string name);
      ("reason", Json.string reason);
    ]
  | UnsupportedExpr { reason } -> Json.obj
    [ ("kind", Json.string "unsupported_expr"); ("reason", Json.string reason); ]

let lower_constant = fun constant ->
  match constant with
  | Core.Constant.Unit -> Nir.Literal.Unit
  | Core.Constant.Bool value -> Nir.Literal.Bool value
  | Core.Constant.Int value -> Nir.Literal.Int value
  | Core.Constant.Float value -> Nir.Literal.Float value
  | Core.Constant.String value -> Nir.Literal.String value

let rec lower_expr = fun expr ->
  match expr with
  | Core.Expr.Constant constant -> ok (Nir.Expr.Literal (lower_constant constant))
  | Core.Expr.Var name -> ok (Nir.Expr.Symbol name)
  | Core.Expr.Apply { callee=Core.Expr.Direct name; arguments } -> Result.map
    (fun arguments -> Nir.Expr.Call Nir.Expr.{ callee = Direct name; arguments })
    (map_results arguments lower_expr)
  | Core.Expr.Apply { callee=Core.Expr.Indirect callee; arguments } -> validation_map2
    (lower_expr callee)
    (map_results arguments lower_expr)
    (fun callee arguments -> Nir.Expr.Call Nir.Expr.{ callee = Indirect callee; arguments })
  | Core.Expr.Lambda _ -> error
    (UnsupportedExpr {
      reason = "nested lambda expressions are outside the first Core IR -> NIR lowering slice"
    })
  | Core.Expr.Let _ -> error
    (UnsupportedExpr {
      reason = "let expressions are outside the first Core IR -> NIR lowering slice"
    })
  | Core.Expr.Sequence _ -> error
    (UnsupportedExpr {
      reason = "sequence expressions are outside the first Core IR -> NIR lowering slice"
    })
  | Core.Expr.If_then_else _ -> error
    (UnsupportedExpr {
      reason = "if expressions are outside the first Core IR -> NIR lowering slice"
    })
  | Core.Expr.Primitive _ -> error
    (UnsupportedExpr {
      reason = "primitive expressions are outside the first Core IR -> NIR lowering slice"
    })

let lower_binding = fun (binding: Core.Binding.t) ->
  match binding.expr with
  | Core.Expr.Lambda lambda -> Result.map
    (fun body -> LoweredFunction Nir.Function.{ name = binding.name; params = lambda.params; body })
    (lower_expr lambda.body)
  | expr -> Result.map
    (fun expr -> LoweredEntry (Nir.Entry_item.Binding Nir.Binding.{ name = binding.name; expr }))
    (lower_expr expr)

let lower_item = fun item ->
  match item with
  | Core.Init_item.Binding binding -> lower_binding binding
  | Core.Init_item.Eval expr -> Result.map
    (fun expr -> LoweredEntry (Nir.Entry_item.Eval expr))
    (lower_expr expr)

let recursive_group_is_function_only = fun (group: Core.Binding_group.t) ->
  List.for_all
    (fun item ->
      match item with
      | Core.Init_item.Binding { expr=Core.Expr.Lambda _; _ } -> true
      | _ -> false)
    group.items

let lower_group = fun group_index (group: Core.Binding_group.t) ->
  match group.rec_flag with
  | Core.Rec_flag.Recursive ->
      if recursive_group_is_function_only group then
        map_results group.items lower_item
      else
        error
          (UnsupportedGroup {
            group_index;
            reason = "recursive groups are only supported when every item is a top-level lambda binding"
          })
  | Core.Rec_flag.Nonrecursive -> map_results group.items lower_item

let lower_export = fun (export: Core.Export.t) ->
  Nir.Export.{ name = export.name; symbol = export.symbol }

let lower_compilation_unit = fun (compilation_unit: Core.Compilation_unit.t) ->
  match compilation_unit.unit_id.kind with
  | Source_unit.Interface -> error (UnsupportedModuleKind { kind = compilation_unit.unit_id.kind })
  | Source_unit.Implementation ->
      let groups =
        List.mapi (fun index group -> (index + 1, group)) compilation_unit.init
      in
      Result.map
        (fun groups ->
          let items = List.flatten groups in
          let functions, entry =
            List.fold_left
              (fun (functions, entry) item ->
                match item with
                | LoweredFunction function_ -> (functions @ [ function_ ], entry)
                | LoweredEntry entry_item -> (functions, entry @ [ entry_item ]))
              ([], [])
              items
          in
          Nir.Program.{
            module_name = compilation_unit.unit_id.unit_name;
            imports = [];
            runtime_helpers = [];
            functions;
            entry;
            exports = List.map lower_export compilation_unit.exports;
          } |> Passes.Normalize.program |> Passes.Simplify.program)
        (map_results groups (fun (group_index, group) -> lower_group group_index group))
