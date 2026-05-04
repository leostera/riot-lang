open Std
open Std.Data
module Core = Raml_core.Core_ir
module Jir = Types
module Calls = Calls
module Intrinsics = Intrinsics
module Objects = Objects
module Primitives = Primitives
module References = References

type error =
  | UnsupportedModuleKind of { kind: Raml_core.Source_unit.kind }
  | UnsupportedGroup of { group_index: int; reason: string }
  | UnsupportedBinding of { name: string; reason: string }
  | UnsupportedExpr of { reason: string }

type 'value validation = ('value, error list) result

let ok = fun value -> Ok value

let error = fun value -> Error [ value ]

let source_kind_to_string = fun kind ->
  match kind with
  | Raml_core.Source_unit.Implementation -> "implementation"
  | Raml_core.Source_unit.Interface -> "interface"

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
    [ ("kind", Json.string "unsupported_expr"); ("reason", Json.string reason) ]

let lower_constant = fun constant ->
  match constant with
  | Core.Constant.Unit -> Jir.Literal.Undefined
  | Core.Constant.Bool value -> Jir.Literal.Bool value
  | Core.Constant.Int value -> Jir.Literal.Number (Jir.Literal.Int value)
  | Core.Constant.Float value -> Jir.Literal.Number (Jir.Literal.Float value)
  | Core.Constant.Char value -> Jir.Literal.String value
  | Core.Constant.String value -> Jir.Literal.String value

let binding_id_of_entity = fun ~fallback_name entity_id ->
  match Core.Entity_id.binding_id entity_id with
  | Some binding_id -> binding_id
  | None ->
      let path = Core.Surface_path.from_segments [ "__raml_js"; "binding"; fallback_name ] in
      Core.Binding_id.persistent path

let binder_of_entity = fun ~fallback_name entity_id ->
  Jir.Binder.make ~name:fallback_name (binding_id_of_entity ~fallback_name entity_id)

let binder_of_param = fun (param: Core.Expr.param) ->
  binder_of_entity ~fallback_name:param.name param.entity_id

let binder_of_binding = fun (binding: Core.Expr.binding) ->
  binder_of_entity ~fallback_name:binding.name binding.entity_id

let binder_of_top_binding = fun (binding: Core.Binding.t) ->
  binder_of_entity ~fallback_name:binding.name binding.entity_id

let iife = fun body ->
  Jir.Expr.Call Jir.Expr.{
    callee = Jir.Expr.Function Jir.Expr.{ params = []; body };
    arguments = []
  }

let lower_curried_function = fun (function_: Jir.Expr.function_) ->
  let arity = List.length function_.params in
  if arity <= 1 then
    Jir.Expr.Function function_
  else
    Intrinsics.call
      (Jir.Expr.Runtime_helper (Jir.Runtime.make_curried ()))
      [ Jir.Expr.Function function_; Jir.Expr.Literal (Jir.Literal.Number (Jir.Literal.Int arity)); ]

let rec lower_expr = fun expr ->
  match expr with
  | Core.Expr.Constant constant ->
      Jir.Expr.Literal (lower_constant constant)
  | Core.Expr.Var entity_id ->
      References.entity entity_id
  | Core.Expr.Apply { callee=Core.Expr.Direct function_name; arguments } ->
      let arguments = List.map arguments ~fn:lower_expr in
      Calls.direct function_name arguments
  | Core.Expr.Apply { callee=Core.Expr.Indirect callee; arguments } ->
      let callee = lower_expr callee in
      let arguments = List.map arguments ~fn:lower_expr in
      Jir.Expr.Call Jir.Expr.{ callee; arguments }
  | Core.Expr.Lambda lambda ->
      lower_curried_function
        Jir.Expr.{
          params = List.map lambda.params ~fn:binder_of_param;
          body = lower_tail_expr lambda.body
        }
  | Core.Expr.Let let_ ->
      lower_let let_
  | Core.Expr.Sequence sequence ->
      iife
        [
          Jir.Statement.Expression (lower_expr sequence.first);
          Jir.Statement.Return (lower_expr sequence.second);
        ]
  | Core.Expr.Tuple tuple ->
      Intrinsics.array (List.map tuple ~fn:lower_expr)
  | Core.Expr.Tuple_get tuple_get ->
      Intrinsics.index
        (lower_expr tuple_get.tuple)
        (Jir.Expr.Literal (Jir.Literal.Number (Jir.Literal.Int tuple_get.index)))
  | Core.Expr.Record record ->
      Objects.literal (List.map record ~fn:lower_record_field)
  | Core.Expr.Record_get record_get ->
      References.named_property_access (lower_expr record_get.record) record_get.label
  | Core.Expr.If_then_else if_then_else ->
      Jir.Expr.Conditional Jir.Expr.{
        condition = lower_expr if_then_else.condition;
        then_ = lower_expr if_then_else.then_;
        else_ = lower_expr if_then_else.else_
      }
  | Core.Expr.Primitive primitive ->
      Primitives.lower primitive.primitive (List.map primitive.arguments ~fn:lower_expr)

and lower_record_field = fun (field: Core.Expr.record_field) ->
  Objects.field field.label (lower_expr field.value)

and lower_tail_expr = fun expr ->
  match expr with
  | Core.Expr.Sequence sequence -> lower_effect_expr sequence.first @ lower_tail_expr sequence.second
  | Core.Expr.Let let_ -> lower_tail_let let_
  | Core.Expr.If_then_else if_then_else -> [
    Jir.Statement.If Jir.Statement.{
      condition = lower_expr if_then_else.condition;
      then_ = lower_tail_expr if_then_else.then_;
      else_ = lower_tail_expr if_then_else.else_
    }
  ]
  | _ -> [ Jir.Statement.Return (lower_expr expr) ]

and lower_effect_expr = fun expr ->
  match expr with
  | Core.Expr.Sequence sequence -> lower_effect_expr sequence.first @ lower_effect_expr sequence.second
  | Core.Expr.If_then_else if_then_else -> [
    Jir.Statement.If Jir.Statement.{
      condition = lower_expr if_then_else.condition;
      then_ = lower_effect_expr if_then_else.then_;
      else_ = lower_effect_expr if_then_else.else_
    }
  ]
  | _ -> [ Jir.Statement.Expression (lower_expr expr) ]

and lower_let_binding_statements = fun (let_: Core.Expr.let_) ->
  match let_.rec_flag with
  | Core.Rec_flag.Nonrecursive -> List.map
    let_.bindings
    ~fn:(fun (binding: Core.Expr.binding) ->
      Jir.Statement.Declaration Jir.Declaration.{
        kind = Jir.Declaration.Const;
        binder = binder_of_binding binding;
        init = Some (lower_expr binding.expr)
      })
  | Core.Rec_flag.Recursive ->
      let prelude =
        List.map
          let_.bindings
          ~fn:(fun (binding: Core.Expr.binding) ->
            Jir.Statement.Declaration Jir.Declaration.{
              kind = Jir.Declaration.Let;
              binder = binder_of_binding binding;
              init = None
            })
      in
      let assignments =
        List.map
          let_.bindings
          ~fn:(fun (binding: Core.Expr.binding) ->
            Jir.Statement.Expression (Jir.Expr.Assignment Jir.Expr.{
              target = binding.entity_id;
              value = lower_expr binding.expr
            }))
          
      in
      prelude @ assignments

and lower_tail_let = fun (let_: Core.Expr.let_) ->
  lower_let_binding_statements let_ @ lower_tail_expr let_.body

and lower_let = fun (let_: Core.Expr.let_) -> iife (lower_tail_let let_)

let lower_export = fun (export: Core.Export.t) ->
  Jir.Export.{ name = export.name; local = export.symbol }

let lower_item = fun item ->
  match item with
  | Core.Init_item.Binding binding -> Jir.Statement.Declaration Jir.Declaration.{
    kind = Jir.Declaration.Const;
    binder = binder_of_top_binding binding;
    init = Some (lower_expr binding.expr)
  }
  | Core.Init_item.Eval expr -> Jir.Statement.Expression (lower_expr expr)

let lower_recursive_group = fun (group: Core.Binding_group.t) ->
  let prelude =
    group.items
    |> List.filter_map
      ~fn:(fun item ->
        match item with
        | Core.Init_item.Binding binding -> Some (Jir.Statement.Declaration Jir.Declaration.{
          kind = Jir.Declaration.Let;
          binder = binder_of_top_binding binding;
          init = None
        })
        | Core.Init_item.Eval _ -> None)
  in
  let body =
    List.map
      group.items
      ~fn:(fun item ->
        match item with
        | Core.Init_item.Binding binding -> Jir.Statement.Expression (Jir.Expr.Assignment Jir.Expr.{
          target = binding.entity_id;
          value = lower_expr binding.expr
        })
        | Core.Init_item.Eval expr -> Jir.Statement.Expression (lower_expr expr))
  in
  prelude @ body

let lower_group = fun (_group_index: int) (group: Core.Binding_group.t) ->
  match group.rec_flag with
  | Core.Rec_flag.Nonrecursive -> List.map group.items ~fn:lower_item
  | Core.Rec_flag.Recursive -> lower_recursive_group group

let lower_compilation_unit = fun ~context (compilation_unit: Core.Compilation_unit.t) ->
  match compilation_unit.unit_id.kind with
  | Raml_core.Source_unit.Interface -> error
    (UnsupportedModuleKind { kind = compilation_unit.unit_id.kind })
  | Raml_core.Source_unit.Implementation ->
      let groups =
        List.enumerate compilation_unit.init
        |> List.map ~fn:(fun (index, group) -> (index + 1, group))
      in
      let body = groups
      |> List.map ~fn:(fun (group_index, group) -> lower_group group_index group)
      |> List.concat in
      let program =
        Jir.Program.{
          module_name = compilation_unit.unit_id.unit_name;
          imports = [];
          body;
          exports = List.map compilation_unit.exports ~fn:lower_export
        } in
      (* Pass order is intentionally explicit:
         - Normalize establishes a canonical structural baseline and recollects
           imports from the freshly lowered body.
         - Flatten exposes statement-shaped work that lowering encoded with
           zero-arg IIFEs.
         - Alpha makes printable names collision-free before later rewrites
           reuse those binders.
         - Remove_aliases and Dce simplify data flow and delete dead local
         scaffolding.
         - Normalize runs again after those rewrites because they can expose
           empty blocks, empty conditionals, or stale import requirements.
         - Dce runs a second time after that normalization because the first DCE
           pass can expose fresh dead declarations only once those empty control
           flow wrappers collapse.
         - Materialize_imports marks the boundary where late JIR stops carrying
           unresolved import/runtime expression nodes.
         - Remove_aliases and Prune_imports run one last time to clean up names
           and imports made redundant by materialization. *)
      let normalized = Passes.Normalize.program ~context program in
      let flattened = Passes.Flatten.program ~context normalized in
      let alpha_renamed = Passes.Alpha.program ~context flattened in
      let aliases_removed = Passes.Remove_aliases.program ~context alpha_renamed in
      let dce_lowered = Passes.Dce.program ~context aliases_removed in
      let normalized_after_dce = Passes.Normalize.program ~context dce_lowered in
      let dce_after_normalize = Passes.Dce.program ~context normalized_after_dce in
      let materialized_imports = Passes.Materialize_imports.program ~context dce_after_normalize in
      let aliases_removed_after_imports = Passes.Remove_aliases.program ~context materialized_imports in
      let imports_pruned = Passes.Prune_imports.program ~context aliases_removed_after_imports in
      ok imports_pruned
