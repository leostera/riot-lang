open Std
open Model

let runtime_args_hole_id = (-2)

let array_length_element_hole_id = (-1)

let monomorphic = fun ty -> TypeScheme.of_type ty

let var = fun id -> TypeRepr.make_var id

let arrow = fun ?(label = TypeRepr.Nolabel) lhs rhs -> TypeRepr.arrow ~label ~lhs ~rhs

let bare_named = fun name ->
  TypeRepr.named ~type_constructor_id:None ~name:(IdentPath.of_name name) ~arguments:[]

let qualified_name = fun module_name name ->
  IdentPath.append_name (IdentPath.of_name module_name) name

let module_typings = fun ~source_id ~module_name exports ->
  let file_summary = FileSummary.trusted ~source_id exports in
  let export_result = file_summary.export_result in
  let type_decls = file_summary.type_decls in
  let source_hash = ModuleTypings.synthetic_source_hash ~module_name ~export_result ~type_decls in
  ModuleTypings.of_file_summary ~module_name ~source_hash file_summary

let int_binop = monomorphic (arrow TypeRepr.int (arrow TypeRepr.int TypeRepr.int))

let float_binop = monomorphic (arrow TypeRepr.float (arrow TypeRepr.float TypeRepr.float))

let polymorphic_list_map =
  let source = var 0 in
  let target = var 1 in
  TypeScheme.of_explicit
    ~quantified:[ 0; 1 ]
    (arrow (arrow source target) (arrow (TypeRepr.list source) (TypeRepr.list target)))

let polymorphic_list_of_seq =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (TypeRepr.seq element) (TypeRepr.list element))

let polymorphic_list_rev =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (TypeRepr.list element) (TypeRepr.list element))

let runtime_args = TypeScheme.of_type (TypeRepr.hole runtime_args_hole_id)

let runtime_run =
  let args = var 0 in
  let exit_reason = var 1 in
  TypeScheme.of_explicit
    ~quantified:[ 1; 0 ]
    (arrow
      ~label:(TypeRepr.Labelled "main")
      (arrow ~label:(TypeRepr.Labelled "args") args (TypeRepr.result TypeRepr.unit_ exit_reason))
      (arrow ~label:(TypeRepr.Labelled "args") args (arrow TypeRepr.unit_ TypeRepr.unit_)))

let summaries = []
|> List.mapi
  (fun index (module_name, exports) ->
    module_typings ~source_id:(SourceId.of_int ((-1_000) - index)) ~module_name exports)
