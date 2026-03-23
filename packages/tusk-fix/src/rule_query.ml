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
  | Some source_file ->
      Syn.Visit.source_file
        {
          Syn.Visit.default with
          enter_expression =
            (fun expressions expression ->
              Syn.Visit.Continue (expression :: expressions));
        }
        [] source_file
      |> List.rev

let let_bindings (ctx : Rule.context) =
  match ctx.cst with
  | None ->
      []
  | Some source_file ->
      Syn.Visit.source_file
        {
          Syn.Visit.default with
          enter_let_binding =
            (fun bindings binding ->
              Syn.Visit.Continue (binding :: bindings));
        }
        [] source_file
      |> List.rev

let type_declarations (ctx : Rule.context) =
  match ctx.cst with
  | None ->
      []
  | Some source_file ->
      Syn.Visit.source_file
        {
          Syn.Visit.default with
          enter_type_declaration =
            (fun declarations declaration ->
              Syn.Visit.Continue (declaration :: declarations));
        }
        [] source_file
      |> List.rev
