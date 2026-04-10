open Std
open Std.Data
module Core = Core_ir
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
  | Core.Constant.String value -> Jir.Literal.String value

let is_ascii_uppercase = fun char -> char >= 'A' && char <= 'Z'

let is_module_segment = fun segment -> String.length segment > 0 && is_ascii_uppercase segment.[0]

let module_import_path = fun module_name -> format Format.[ str "./"; str module_name; str ".js" ]

let primitive_dispatch = fun () ->
  Jir.Runtime.make
    ~module_name:"@riot/raml/js/runtime"
    ~symbol:"callPrimitive"
    ~local:"__callPrimitive"
    ()

let lower_reference = fun name ->
  let parts = String.split_on_char '.' name |> List.filter (fun part -> not (String.equal part "")) in
  match parts with
  | [] -> Jir.Expr.Identifier name
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

let rec lower_expr = fun expr ->
  match expr with
  | Core.Expr.Constant constant ->
      Jir.Expr.Literal (lower_constant constant)
  | Core.Expr.Var name ->
      lower_reference name
  | Core.Expr.Apply { callee=Core.Expr.Direct function_name; arguments } ->
      let callee = lower_reference function_name in
      let arguments = List.map lower_expr arguments in
      Jir.Expr.Call Jir.Expr.{ callee; arguments }
  | Core.Expr.Apply { callee=Core.Expr.Indirect callee; arguments } ->
      let callee = lower_expr callee in
      let arguments = List.map lower_expr arguments in
      Jir.Expr.Call Jir.Expr.{ callee; arguments }
  | Core.Expr.Lambda lambda ->
      Jir.Expr.Function Jir.Expr.{
        params = lambda.params;
        body = [ Jir.Statement.Return (lower_expr lambda.body) ]
      }
  | Core.Expr.Let let_ ->
      lower_let let_
  | Core.Expr.Sequence sequence ->
      iife
        [
          Jir.Statement.Expression (lower_expr sequence.first);
          Jir.Statement.Return (lower_expr sequence.second);
        ]
  | Core.Expr.If_then_else if_then_else ->
      Jir.Expr.Conditional Jir.Expr.{
        condition = lower_expr if_then_else.condition;
        then_ = lower_expr if_then_else.then_;
        else_ = lower_expr if_then_else.else_
      }
  | Core.Expr.Primitive primitive ->
      let callee = Jir.Expr.Runtime_helper (primitive_dispatch ()) in
      let arguments = Jir.Expr.Literal (Jir.Literal.String primitive.name)
      :: List.map lower_expr primitive.arguments in
      Jir.Expr.Call Jir.Expr.{ callee; arguments }

and lower_let = fun (let_: Core.Expr.let_) ->
  let statements =
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
  in
  iife (statements @ [ Jir.Statement.Return (lower_expr let_.body) ])

let lower_export = fun (export: Core.Export.t) ->
  Jir.Export.{ name = export.name; local = export.symbol }

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
        |> Passes.Normalize.program)
