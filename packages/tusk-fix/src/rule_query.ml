open Std

let structure_items (ctx : Rule.context) =
  match ctx.cst with
  | Some (Syn.Cst.Implementation implementation) ->
      implementation.items
  | Some (Syn.Cst.Interface _) | None ->
      []

let signature_items (ctx : Rule.context) =
  match ctx.cst with
  | Some (Syn.Cst.Interface interface) ->
      interface.items
  | Some (Syn.Cst.Implementation _) | None ->
      []

let expressions (ctx : Rule.context) =
  match ctx.cst with
  | None ->
      []
  | Some cst_file ->
      Syn.Visit.source_file
        {
          Syn.Visit.default with
          visit_expression =
            (fun expressions walk expression ->
              walk.descend_expression (expression :: expressions) expression);
        }
        [] cst_file
      |> List.rev

let let_bindings (ctx : Rule.context) =
  match ctx.cst with
  | None ->
      []
  | Some cst_file ->
      Syn.Visit.source_file
        {
          Syn.Visit.default with
          visit_let_binding =
            (fun bindings walk binding ->
              walk.descend_let_binding (binding :: bindings) binding);
        }
        [] cst_file
      |> List.rev

let type_declarations (ctx : Rule.context) =
  match ctx.cst with
  | None ->
      []
  | Some cst_file ->
      Syn.Visit.source_file
        {
          Syn.Visit.default with
          visit_type_declaration =
            (fun declarations walk declaration ->
              walk.descend_type_declaration (declaration :: declarations)
                declaration);
        }
        [] cst_file
      |> List.rev
