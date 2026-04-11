open Std
open Std.Data
module Source_unit = Raml_core.Source_unit
module Core = Raml_core.Core_ir
module Jir = Types

type error =
  | UnsupportedModuleKind of { kind: Source_unit.kind }
  | UnsupportedGroup of { group_index: int; reason: string }
  | UnsupportedBinding of { name: string; reason: string }
  | UnsupportedExpr of { reason: string }

type 'value validation = ('value, error list) result

let ok = fun value -> Ok value

let error = fun value -> Error [ value ]

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
  | Core.Constant.Unit -> Jir.Literal.Undefined
  | Core.Constant.Bool value -> Jir.Literal.Bool value
  | Core.Constant.Int value -> Jir.Literal.Number (Jir.Literal.Int value)
  | Core.Constant.Float value -> Jir.Literal.Number (Jir.Literal.Float value)
  | Core.Constant.Char value -> Jir.Literal.String value
  | Core.Constant.String value -> Jir.Literal.String value

let is_ascii_uppercase = fun char -> char >= 'A' && char <= 'Z'

let is_module_segment = fun segment -> String.length segment > 0 && is_ascii_uppercase segment.[0]

let module_import_path = fun module_name -> format Format.[ str "./"; str module_name; str ".js" ]

let param_name = fun (param: Core.Expr.param) -> param.name

let unresolved_bare_name = fun entity_id ->
  match Core.Entity_id.binding_id entity_id with
  | Some _ -> None
  | None ->
      if Core.Entity_id.is_bare entity_id then
        Core.Entity_id.bare_name entity_id
      else
        None

let lower_reference = fun entity_id ->
  let parts = Core.Entity_id.to_segments entity_id in
  match parts with
  | [] -> Jir.Expr.Identifier (Core.Entity_id.to_string entity_id)
  | head :: tail ->
      let base =
        if not (List.is_empty tail) && is_module_segment head then
          Jir.Expr.Imported (Jir.Imports.namespace ~from:(module_import_path head) ~local:head ())
        else
          Jir.Expr.Identifier head
      in
      List.fold_left (fun object_ property -> Jir.Expr.Member Jir.Expr.{ object_; property }) base tail

let iife = fun body ->
  Jir.Expr.Call Jir.Expr.{
    callee = Jir.Expr.Function Jir.Expr.{ params = []; body };
    arguments = []
  }

let lower_direct_callee = fun entity_id ->
  match unresolved_bare_name entity_id with
  | Some function_name -> (
      match Jir.Runtime.helper_for_direct_callee function_name with
      | Some helper -> Jir.Expr.Runtime_helper helper
      | None -> lower_reference entity_id
    )
  | None -> lower_reference entity_id

let lower_runtime_primitive_call = fun name arguments ->
  let callee = Jir.Expr.Runtime_helper (Jir.Runtime.call_primitive ()) in
  let arguments = Jir.Expr.Literal (Jir.Literal.String name) :: arguments in
  Jir.Expr.Call Jir.Expr.{ callee; arguments }

let lower_bool = fun value -> Jir.Expr.Literal (Jir.Literal.Bool value)

let lower_curried_function = fun (function_: Jir.Expr.function_) ->
  let arity = List.length function_.params in
  if arity <= 1 then
    Jir.Expr.Function function_
  else
    Jir.Expr.Call Jir.Expr.{
      callee = Jir.Expr.Runtime_helper (Jir.Runtime.make_curried ());
      arguments = [
        Jir.Expr.Function function_;
        Jir.Expr.Literal (Jir.Literal.Number (Jir.Literal.Int arity));
      ]
    }

let primitive_for_direct_callee = fun entity_id ->
  match unresolved_bare_name entity_id with
  | Some function_name -> (
      match function_name with
      | "+." -> Some "%addfloat"
      | "-." -> Some "%subfloat"
      | "*." -> Some "%mulfloat"
      | "/." -> Some "%divfloat"
      | "=" -> Some "%eq"
      | "<>" -> Some "%neq"
      | "<" -> Some "%lt"
      | "<=" -> Some "%le"
      | ">" -> Some "%gt"
      | ">=" -> Some "%ge"
      | "+" -> Some "%addint"
      | "-" -> Some "%subint"
      | "*" -> Some "%mulint"
      | "/" -> Some "%divint"
      | "mod" -> Some "%modint"
      | "^" -> Some "%concatstring"
      | "sqrt" -> Some "%sqrtfloat"
      | "string_of_int" -> Some "%string_of_int"
      | "string_of_float" -> Some "%string_of_float"
      | "int_of_string" -> Some "%int_of_string"
      | "float_of_string" -> Some "%float_of_string"
      | _ -> None
    )
  | None -> None

let lower_boolean_direct_call = fun entity_id arguments ->
  match (unresolved_bare_name entity_id, arguments) with
  | (Some "not", [ argument ]) -> Some (Jir.Expr.Conditional Jir.Expr.{
    condition = argument;
    then_ = lower_bool false;
    else_ = lower_bool true
  })
  | (Some "&&", [left;right]) -> Some (Jir.Expr.Conditional Jir.Expr.{
    condition = left;
    then_ = right;
    else_ = lower_bool false
  })
  | (Some "||", [left;right]) -> Some (Jir.Expr.Conditional Jir.Expr.{
    condition = left;
    then_ = lower_bool true;
    else_ = right
  })
  | _ -> None

let lower_direct_call = fun entity_id arguments ->
  match lower_boolean_direct_call entity_id arguments with
  | Some expr -> expr
  | None ->
      match primitive_for_direct_callee entity_id with
      | Some primitive_name -> lower_runtime_primitive_call primitive_name arguments
      | None ->
          let callee = lower_direct_callee entity_id in
          Jir.Expr.Call Jir.Expr.{ callee; arguments }

let rec lower_expr = fun expr ->
  match expr with
  | Core.Expr.Constant constant ->
      Jir.Expr.Literal (lower_constant constant)
  | Core.Expr.Var entity_id ->
      lower_reference entity_id
  | Core.Expr.Apply { callee=Core.Expr.Direct function_name; arguments } ->
      let arguments = List.map lower_expr arguments in
      lower_direct_call function_name arguments
  | Core.Expr.Apply { callee=Core.Expr.Indirect callee; arguments } ->
      let callee = lower_expr callee in
      let arguments = List.map lower_expr arguments in
      Jir.Expr.Call Jir.Expr.{ callee; arguments }
  | Core.Expr.Lambda lambda ->
      lower_curried_function
        Jir.Expr.{ params = List.map param_name lambda.params; body = lower_tail_expr lambda.body }
  | Core.Expr.Let let_ ->
      lower_let let_
  | Core.Expr.Sequence sequence ->
      iife
        [
          Jir.Statement.Expression (lower_expr sequence.first);
          Jir.Statement.Return (lower_expr sequence.second);
        ]
  | Core.Expr.Tuple tuple ->
      lower_runtime_primitive_call "%tuple_make" (List.map lower_expr tuple)
  | Core.Expr.Tuple_get tuple_get ->
      lower_runtime_primitive_call
        "%tuple_get"
        [
          lower_expr tuple_get.tuple;
          Jir.Expr.Literal (Jir.Literal.Number (Jir.Literal.Int tuple_get.index));
        ]
  | Core.Expr.If_then_else if_then_else ->
      Jir.Expr.Conditional Jir.Expr.{
        condition = lower_expr if_then_else.condition;
        then_ = lower_expr if_then_else.then_;
        else_ = lower_expr if_then_else.else_
      }
  | Core.Expr.Primitive primitive ->
      lower_runtime_primitive_call primitive.name (List.map lower_expr primitive.arguments)

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
    (fun (binding: Core.Expr.binding) ->
      Jir.Statement.Declaration Jir.Declaration.{
        kind = Jir.Declaration.Const;
        name = binding.name;
        init = Some (lower_expr binding.expr)
      })
    let_.bindings
  | Core.Rec_flag.Recursive ->
      let prelude =
        List.map
          (fun (binding: Core.Expr.binding) ->
            Jir.Statement.Declaration Jir.Declaration.{
              kind = Jir.Declaration.Let;
              name = binding.name;
              init = None
            })
          let_.bindings
      in
      let assignments =
        List.map
          (fun (binding: Core.Expr.binding) ->
            Jir.Statement.Expression (Jir.Expr.Assignment Jir.Expr.{
              target = binding.name;
              value = lower_expr binding.expr
            }))
          let_.bindings
      in
      prelude @ assignments

and lower_tail_let = fun (let_: Core.Expr.let_) ->
  lower_let_binding_statements let_ @ lower_tail_expr let_.body

and lower_let = fun (let_: Core.Expr.let_) -> iife (lower_tail_let let_)

let lower_export = fun (export: Core.Export.t) ->
  Jir.Export.{ name = export.name; local = Core.Entity_id.to_string export.symbol }

let lower_item = fun item ->
  match item with
  | Core.Init_item.Binding binding -> Jir.Statement.Declaration Jir.Declaration.{
    kind = Jir.Declaration.Const;
    name = binding.name;
    init = Some (lower_expr binding.expr)
  }
  | Core.Init_item.Eval expr -> Jir.Statement.Expression (lower_expr expr)

let lower_recursive_group = fun (group: Core.Binding_group.t) ->
  let prelude =
    group.items
    |> List.filter_map
      (fun item ->
        match item with
        | Core.Init_item.Binding binding -> Some (Jir.Statement.Declaration Jir.Declaration.{
          kind = Jir.Declaration.Let;
          name = binding.name;
          init = None
        })
        | Core.Init_item.Eval _ -> None)
  in
  let body =
    List.map
      (fun item ->
        match item with
        | Core.Init_item.Binding binding -> Jir.Statement.Expression (Jir.Expr.Assignment Jir.Expr.{
          target = binding.name;
          value = lower_expr binding.expr
        })
        | Core.Init_item.Eval expr -> Jir.Statement.Expression (lower_expr expr))
      group.items
  in
  prelude @ body

let lower_group = fun (_group_index: int) (group: Core.Binding_group.t) ->
  match group.rec_flag with
  | Core.Rec_flag.Nonrecursive -> List.map lower_item group.items
  | Core.Rec_flag.Recursive -> lower_recursive_group group

let lower_compilation_unit = fun (compilation_unit: Core.Compilation_unit.t) ->
  match compilation_unit.unit_id.kind with
  | Source_unit.Interface -> error (UnsupportedModuleKind { kind = compilation_unit.unit_id.kind })
  | Source_unit.Implementation ->
      let groups =
        List.mapi (fun index group -> (index + 1, group)) compilation_unit.init
      in
      let body = groups
      |> List.map (fun (group_index, group) -> lower_group group_index group)
      |> List.flatten in
      ok
        (Jir.Program.{
          module_name = compilation_unit.unit_id.unit_name;
          imports = [];
          body;
          exports = List.map lower_export compilation_unit.exports
        }
        |> Passes.Normalize.program
        |> Passes.Flatten.program
        |> Passes.Alpha.program
        |> Passes.Remove_aliases.program
        |> Passes.Dce.program
        |> Passes.Normalize.program
        |> Passes.Materialize_imports.program)
