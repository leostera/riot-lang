open Std

let structure_items (ctx : Rule.context) =
  match ctx.cst with
  | Syn.Cst.Implementation implementation ->
      implementation.items
  | Syn.Cst.Interface _ ->
      []

let signature_items (ctx : Rule.context) =
  match ctx.cst with
  | Syn.Cst.Interface interface ->
      interface.items
  | Syn.Cst.Implementation _ ->
      []

let expressions (ctx : Rule.context) =
  Syn.Visit.source_file
    {
      Syn.Visit.default with
      visit_expression =
        (fun expressions walk expression ->
          walk.descend_expression (expression :: expressions) expression);
    }
    [] ctx.cst
  |> List.rev

let let_bindings (ctx : Rule.context) =
  Syn.Visit.source_file
    {
      Syn.Visit.default with
      visit_let_binding =
        (fun bindings walk binding ->
          walk.descend_let_binding (binding :: bindings) binding);
    }
    [] ctx.cst
  |> List.rev

let type_declarations (ctx : Rule.context) =
  Syn.Visit.source_file
    {
      Syn.Visit.default with
      visit_type_declaration =
        (fun declarations walk declaration ->
          walk.descend_type_declaration (declaration :: declarations)
            declaration);
    }
    [] ctx.cst
  |> List.rev
