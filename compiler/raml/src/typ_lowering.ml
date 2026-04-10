open Std
open Std.Data
open Typ.Model
module Core = Core_ir

type error =
  | UnsupportedSourceKind of { kind: Source_unit.kind }
  | UnsupportedItem of { item_id: ItemArenaId.t; kind: string; scope_path: SurfacePath.t }
  | MissingBinding of { binding_id: BindingArenaId.t }
  | MissingExpr of { expr_id: ExprArenaId.t }
  | MissingPattern of { pattern_id: PatternArenaId.t }
  | UnsupportedBinding of { binding_id: BindingArenaId.t; reason: string }
  | UnsupportedPattern of { pattern_id: PatternArenaId.t; reason: string }
  | UnsupportedExpr of { expr_id: ExprArenaId.t; reason: string }
  | InvalidIntLiteral of { expr_id: ExprArenaId.t; literal: string }
  | InvalidFloatLiteral of { expr_id: ExprArenaId.t; literal: string }

type 'value validation = ('value, error list) result

type lowered_binding = {
  export: Core.Export.t option;
  item: Core.Init_item.t;
}

type top_level_pattern =
  | Named of string
  | Unit

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
  | UnsupportedSourceKind { kind } ->
      Json.obj
        [
          ("kind", Json.string "unsupported_source_kind");
          ("source_kind", Json.string (source_kind_to_string kind));
        ]
  | UnsupportedItem { item_id; kind; scope_path } ->
      let _ = item_id in
      Json.obj
        [
          ("kind", Json.string "unsupported_item");
          ("item_kind", Json.string kind);
          ("scope_path", Json.string (SurfacePath.to_string scope_path));
        ]
  | MissingBinding { binding_id } ->
      let _ = binding_id in
      Json.obj [ ("kind", Json.string "missing_binding") ]
  | MissingExpr { expr_id } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "missing_expr") ]
  | MissingPattern { pattern_id } ->
      let _ = pattern_id in
      Json.obj [ ("kind", Json.string "missing_pattern") ]
  | UnsupportedBinding { binding_id; reason } ->
      let _ = binding_id in
      Json.obj [ ("kind", Json.string "unsupported_binding"); ("reason", Json.string reason); ]
  | UnsupportedPattern { pattern_id; reason } ->
      let _ = pattern_id in
      Json.obj [ ("kind", Json.string "unsupported_pattern"); ("reason", Json.string reason); ]
  | UnsupportedExpr { expr_id; reason } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "unsupported_expr"); ("reason", Json.string reason); ]
  | InvalidIntLiteral { expr_id; literal } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "invalid_int_literal"); ("literal", Json.string literal); ]
  | InvalidFloatLiteral { expr_id; literal } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "invalid_float_literal"); ("literal", Json.string literal); ]

let item_kind = fun (item: ItemTree.item) ->
  match item with
  | ItemTree.Type _ -> "type"
  | ItemTree.Exception _ -> "exception"
  | ItemTree.ExtensionConstructor _ -> "extension_constructor"
  | ItemTree.Value _ -> "value"
  | ItemTree.DeclaredValue _ -> "declared_value"
  | ItemTree.Open _ -> "open"
  | ItemTree.Include _ -> "include"
  | ItemTree.ModuleAlias _ -> "module_alias"
  | ItemTree.Unsupported _ -> "unsupported"

let label_to_string = fun label ->
  match label with
  | BodyArena.Positional -> "positional"
  | BodyArena.Labeled name -> format Format.[ str "labeled:"; str name ]
  | BodyArena.Optional name -> format Format.[ str "optional:"; str name ]

let rec lower_expr = fun semantic_tree expr_id ->
  match SemanticTree.find_expr semantic_tree expr_id with
  | None -> error (MissingExpr { expr_id })
  | Some expr -> (
      match expr.desc with
      | BodyArena.EUnit ->
          ok (Core.Expr.Constant Core.Constant.Unit)
      | BodyArena.EBool value ->
          ok (Core.Expr.Constant (Core.Constant.Bool value))
      | BodyArena.EInt literal -> (
          match int_of_string_opt literal with
          | Some value -> ok (Core.Expr.Constant (Core.Constant.Int value))
          | None -> error (InvalidIntLiteral { expr_id; literal })
        )
      | BodyArena.EFloat literal -> (
          match float_of_string_opt literal with
          | Some value -> ok (Core.Expr.Constant (Core.Constant.Float value))
          | None -> error (InvalidFloatLiteral { expr_id; literal })
        )
      | BodyArena.EString value ->
          ok (Core.Expr.Constant (Core.Constant.String value))
      | BodyArena.EVar path ->
          ok (Core.Expr.Var (SurfacePath.to_string path))
      | BodyArena.EApply (callee_id, arguments) ->
          let lower_argument (argument: BodyArena.apply_argument) =
            if argument.implicit then
              error
                (UnsupportedExpr {
                  expr_id;
                  reason = "implicit application arguments are not supported"
                })
            else
              match argument.label with
              | BodyArena.Positional -> lower_expr semantic_tree argument.value_id
              | label -> error
                (UnsupportedExpr {
                  expr_id;
                  reason = format
                    Format.[
                      str "non-positional application arguments are not supported: ";
                      str (label_to_string label);
                    ]
                })
          in
          let lower_callee callee_id =
            match SemanticTree.find_expr semantic_tree callee_id with
            | Some { desc=BodyArena.EVar path; _ } -> ok
              (Core.Expr.Direct (SurfacePath.to_string path))
            | Some _ -> Result.map
              (fun callee -> Core.Expr.Indirect callee)
              (lower_expr semantic_tree callee_id)
            | None -> error (MissingExpr { expr_id = callee_id })
          in
          let arguments = map_results arguments lower_argument in
          let callee = lower_callee callee_id in
          validation_map2
            callee
            arguments
            (fun callee arguments -> Core.Expr.Apply Core.Expr.{ callee; arguments })
      | BodyArena.EChar _ ->
          error
            (UnsupportedExpr {
              expr_id;
              reason = "char literals are not supported in Raml Core IR yet"
            })
      | BodyArena.EFun _
      | BodyArena.ETuple _
      | BodyArena.EArray _
      | BodyArena.ESequence _
      | BodyArena.EFor _
      | BodyArena.EWhile _
      | BodyArena.ERecord _
      | BodyArena.EFieldAccess _
      | BodyArena.EFieldAssign _
      | BodyArena.EIndex _
      | BodyArena.ELet _
      | BodyArena.EIf _
      | BodyArena.EMatch _
      | BodyArena.ETry _
      | BodyArena.EPolyVariant _
      | BodyArena.ECoerce _
      | BodyArena.EModulePack _
      | BodyArena.ELocalModulePack _
      | BodyArena.ELocalModule _
      | BodyArena.ELocalOpen _
      | BodyArena.EUnsupported _
      | BodyArena.EHole _ ->
          error
            (UnsupportedExpr {
              expr_id;
              reason = "expression form is outside the first Typ -> Raml lowering slice"
            })
    )

let lower_var_pattern = fun semantic_tree pattern_id ->
  match SemanticTree.find_pattern semantic_tree pattern_id with
  | None -> error (MissingPattern { pattern_id })
  | Some pattern -> (
      match pattern.desc with
      | BodyArena.PVar name -> ok name
      | _ -> error
        (UnsupportedPattern {
          pattern_id;
          reason = "only variable binders are supported in the first Typ -> Raml lowering slice"
        })
    )

let lower_top_level_pattern = fun semantic_tree pattern_id ->
  match SemanticTree.find_pattern semantic_tree pattern_id with
  | None -> error (MissingPattern { pattern_id })
  | Some pattern -> (
      match pattern.desc with
      | BodyArena.PVar name -> ok (Named name)
      | BodyArena.PUnit -> ok Unit
      | _ -> error
        (UnsupportedPattern {
          pattern_id;
          reason = "only variable and unit top-level binders are supported in the first Typ -> Raml lowering slice"
        })
    )

let lower_function = fun semantic_tree ~name expr_id parameters body_id ->
  let lower_parameter (parameter: BodyArena.function_parameter) =
    if Option.is_some parameter.default_value_id then
      error
        (UnsupportedExpr { expr_id; reason = "default-valued function parameters are not supported" })
    else
      match parameter.label with
      | BodyArena.Positional -> lower_var_pattern semantic_tree parameter.pattern_id
      | label -> error
        (UnsupportedExpr {
          expr_id;
          reason = format
            Format.[
              str "non-positional function parameters are not supported: ";
              str (label_to_string label);
            ]
        })
  in
  let params = map_results parameters lower_parameter in
  let body = lower_expr semantic_tree body_id in
  let _ = name in
  validation_map2 params body (fun params body -> Core.Expr.{ params; body })

let lower_binding = fun semantic_tree binding_id ->
  match SemanticTree.find_binding semantic_tree binding_id with
  | None -> error (MissingBinding { binding_id })
  | Some (binding: BodyArena.binding) ->
      if SurfacePath.is_empty binding.scope_path then
        let name =
          match binding.name with
          | Some name -> ok name
          | None -> ok ""
        in
        let pattern_name = lower_top_level_pattern semantic_tree binding.pattern_id in
        let named_binding =
          Result.and_then (validation_map2
            name
            pattern_name
            (fun name pattern_name -> (name, pattern_name)))
            (fun (name, pattern_name) ->
              match pattern_name with
              | Named pattern_name ->
                  if String.equal name pattern_name then
                    ok (Some name)
                  else
                    error
                      (UnsupportedBinding {
                        binding_id;
                        reason = "binding name and variable pattern must match"
                      })
              | Unit ->
                  if String.equal name "" then
                    ok None
                  else
                    error
                      (UnsupportedBinding {
                        binding_id;
                        reason = "unit-pattern top-level bindings must stay anonymous"
                      }))
        in
        match SemanticTree.find_expr semantic_tree binding.value_id with
        | None -> error (MissingExpr { expr_id = binding.value_id })
        | Some { desc=BodyArena.EFun (parameters, body_id); expr_id; _ } ->
            Result.and_then named_binding
              (
                function
                | None -> error
                  (UnsupportedBinding {
                    binding_id;
                    reason = "unit-pattern top-level bindings cannot introduce functions"
                  })
                | Some name -> Result.map
                  (fun lambda ->
                    {
                      export = Some Core.Export.{ name; symbol = name };
                      item = Core.Init_item.Binding Core.Binding.{
                        name;
                        expr = Core.Expr.Lambda lambda
                      }
                    })
                  (lower_function semantic_tree ~name expr_id parameters body_id)
              )
        | Some _ ->
            Result.and_then named_binding
              (
                function
                | Some name -> Result.map
                  (fun expr ->
                    {
                      export = Some Core.Export.{ name; symbol = name };
                      item = Core.Init_item.Binding Core.Binding.{ name; expr }
                    })
                  (lower_expr semantic_tree binding.value_id)
                | None -> Result.map
                  (fun expr -> { export = None; item = Core.Init_item.Eval expr })
                  (lower_expr semantic_tree binding.value_id)
              )
      else
        error
          (UnsupportedBinding {
            binding_id;
            reason = "nested-scope bindings are not supported in the first Typ -> Raml lowering slice"
          })

let lower_value_item = fun semantic_tree (value_item: ItemTree.value_item) ->
  let bindings = map_results value_item.binding_ids (lower_binding semantic_tree) in
  Result.map
    (fun bindings ->
      let exports =
        List.fold_right
          (fun (binding: lowered_binding) acc ->
            match binding.export with
            | Some export -> export :: acc
            | None -> acc)
          bindings
          []
      in
      let items = bindings |> List.map (fun (binding: lowered_binding) -> binding.item) in
      Core.Binding_group.{
        rec_flag =
          if value_item.recursive then
            Recursive
          else
            Nonrecursive;
        items;
        exports;
      })
    bindings

let lower_item = fun semantic_tree (item: ItemTree.item) ->
  match item with
  | ItemTree.Value value_item when SurfacePath.is_empty value_item.scope_path -> lower_value_item
    semantic_tree
    value_item
  | ItemTree.Value value_item -> error
    (UnsupportedItem {
      item_id = value_item.item_id;
      kind = item_kind item;
      scope_path = value_item.scope_path
    })
  | ItemTree.Type type_item -> error
    (UnsupportedItem {
      item_id = type_item.item_id;
      kind = item_kind item;
      scope_path = type_item.scope_path
    })
  | ItemTree.Exception exception_item -> error
    (UnsupportedItem {
      item_id = exception_item.item_id;
      kind = item_kind item;
      scope_path = exception_item.scope_path
    })
  | ItemTree.ExtensionConstructor extension_item -> error
    (UnsupportedItem {
      item_id = extension_item.item_id;
      kind = item_kind item;
      scope_path = extension_item.scope_path
    })
  | ItemTree.DeclaredValue declared_value_item -> error
    (UnsupportedItem {
      item_id = declared_value_item.item_id;
      kind = item_kind item;
      scope_path = declared_value_item.scope_path
    })
  | ItemTree.Open open_item -> error
    (UnsupportedItem {
      item_id = open_item.item_id;
      kind = item_kind item;
      scope_path = open_item.scope_path
    })
  | ItemTree.Include include_item -> error
    (UnsupportedItem {
      item_id = include_item.item_id;
      kind = item_kind item;
      scope_path = include_item.scope_path
    })
  | ItemTree.ModuleAlias module_alias_item -> error
    (UnsupportedItem {
      item_id = module_alias_item.item_id;
      kind = item_kind item;
      scope_path = module_alias_item.scope_path
    })
  | ItemTree.Unsupported unsupported_item -> error
    (UnsupportedItem {
      item_id = unsupported_item.item_id;
      kind = item_kind item;
      scope_path = unsupported_item.scope_path
    })

let lower_file = fun ~(source_unit:Source_unit.t) (semantic_tree: SemanticTree.file) ->
  match source_unit.kind with
  | Source_unit.Interface -> error (UnsupportedSourceKind { kind = source_unit.kind })
  | Source_unit.Implementation ->
      let lowered_groups = map_results
        (ItemTree.items semantic_tree.item_tree)
        (lower_item semantic_tree) in
      Result.map
        (fun lowered_groups ->
          let exports =
            List.fold_right
              (fun (group: Core.Binding_group.t) acc -> group.exports @ acc)
              lowered_groups
              []
          in
          Core.Compilation_unit.{
            unit_id = Core.Unit_id.of_source_unit source_unit;
            exports;
            init = lowered_groups
          })
        lowered_groups
