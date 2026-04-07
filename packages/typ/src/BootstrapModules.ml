open Std
open Model

let runtime_args_hole_id = (-2)

let array_length_element_hole_id = (-1)

let monomorphic = fun ty -> TypeScheme.of_type ty

let var = fun id -> TypeRepr.make_var id

let arrow = fun ?(label = TypeRepr.Nolabel) lhs rhs -> TypeRepr.arrow ~label ~lhs ~rhs

let bare_named = fun name ->
  let path = IdentPath.of_name name in
  let head =
    match BuiltinTypeConstructors.head_of_path path with
    | Some head -> head
    | None -> raise (Failure ("missing builtin type head " ^ IdentPath.to_string path))
  in
  TypeRepr.named ~head ~arguments:[]

let qualified_name = fun module_name name ->
  IdentPath.append_name (IdentPath.of_name module_name) name

let type_decl = fun ~scope_path declaration ->
  { FileSummary.scope_path; declaration }

let module_typings = fun ~source_id ~module_name ?(type_decls = []) exports ->
  let file_summary = FileSummary.trusted ~source_id ~type_decls exports in
  let export_result = file_summary.export_result in
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

let stdlib_seq_t_decl =
  let element = var 0 in
  type_decl
    ~scope_path:(IdentPath.of_name "Seq")
    {
      TypeDecl.type_constructor_id = TypeConstructorId.of_path (IdentPath.of_string "Stdlib.Seq.t");
      type_name = "t";
      param_ids = [ 0 ];
      param_variances = [ TypeDecl.Covariant ];
      constructors = [];
      labels = [];
      manifest = Some (TypeDecl.Alias (TypeRepr.seq element));
    }

let int_to_string = TypeScheme.of_type (arrow TypeRepr.int TypeRepr.string)

let int_of_string = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.int)

let string_to_seq = TypeScheme.of_type (arrow TypeRepr.string (TypeRepr.seq TypeRepr.char))

let string_make =
  TypeScheme.of_type (arrow TypeRepr.int (arrow TypeRepr.char TypeRepr.string))

let string_starts_with =
  TypeScheme.of_type
    (arrow
      ~label:(TypeRepr.Labelled "prefix")
      TypeRepr.string
      (arrow TypeRepr.string TypeRepr.bool))

let summaries = [
  ("Stdlib", [], [ stdlib_seq_t_decl ]);
  ("Int", [ ("to_string", int_to_string); ("of_string", int_of_string) ], []);
  ("String", [
    ("to_seq", string_to_seq);
    ("make", string_make);
    ("starts_with", string_starts_with);
  ], []);
  ("List", [ ("map", polymorphic_list_map); ("of_seq", polymorphic_list_of_seq); ("rev", polymorphic_list_rev) ], []);
]
|> List.mapi
  (fun index (module_name, exports, type_decls) ->
    module_typings ~source_id:(SourceId.of_int ((-1_000) - index)) ~module_name ~type_decls exports)
