open Std
open Model

let runtime_args_hole_id = (-2)

let array_length_element_hole_id = (-1)

let monomorphic = fun ty -> TypeScheme.of_type ty

let var = fun id -> TypeRepr.make_var id

let arrow = fun ?(label = TypeRepr.Nolabel) lhs rhs -> TypeRepr.arrow ~label ~lhs ~rhs

let named_with_type_constructor_id = fun ~type_constructor_id name ->
  let path = IdentPath.of_name name in
  let head = TypeRepr.named_head ~type_constructor_id ~name:path in
  TypeRepr.named ~head ~arguments:[]

let named_path = fun path -> TypeRepr.named_path ~name:(IdentPath.of_string path) ~arguments:[]

let qualified_name = fun module_name name ->
  IdentPath.append_name (IdentPath.of_name module_name) name

let qualified_export_name = fun module_name name -> qualified_name module_name name |> IdentPath.to_string

let type_decl = fun ~scope_path declaration -> { FileSummary.scope_path; declaration }

let module_typings = fun ~source_id ~module_name ?(type_decls = []) exports ->
  let file_summary = FileSummary.trusted ~source_id ~type_decls exports in
  let export_result = file_summary.export_result in
  let source_hash = ModuleTypings.synthetic_source_hash ~module_name ~export_result ~type_decls () in
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

let polymorphic_list_for_all =
  let element = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow (arrow element TypeRepr.bool) (arrow (TypeRepr.list element) TypeRepr.bool))

let polymorphic_list_filter_map =
  let source = var 0 in
  let target = var 1 in
  TypeScheme.of_explicit
    ~quantified:[ 0; 1 ]
    (arrow
      (arrow source (TypeRepr.option target))
      (arrow (TypeRepr.list source) (TypeRepr.list target)))

let polymorphic_array_length =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (TypeRepr.array element) TypeRepr.int)

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
  type_decl ~scope_path:(IdentPath.of_name "Seq")
    {
      TypeDecl.type_constructor_id = TypeConstructorId.of_path (IdentPath.of_string "Stdlib.Seq.t");
      type_name = "t";
      nonrec_ = false;
      param_ids = [ 0 ];
      param_variances = [ TypeDecl.Covariant ];
      constructors = [];
      labels = [];
      manifest = Some (TypeDecl.Alias (TypeRepr.seq element));
    }

let abstract_type_decl = fun ~scope_path ~path ~type_name ->
  type_decl ~scope_path
    {
      TypeDecl.type_constructor_id = TypeConstructorId.of_path (IdentPath.of_string path);
      type_name;
      nonrec_ = false;
      param_ids = [];
      param_variances = [];
      constructors = [];
      labels = [];
      manifest = None;
    }

let stdlib_buffer_t_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_name "Buffer")
  ~path:"Stdlib.Buffer.t"
  ~type_name:"t"

let buffer_t_decl = abstract_type_decl ~scope_path:IdentPath.empty ~path:"Buffer.t" ~type_name:"t"

let int_to_string = TypeScheme.of_type (arrow TypeRepr.int TypeRepr.string)

let int_of_string = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.int)

let float_of_int = TypeScheme.of_type (arrow TypeRepr.int TypeRepr.float)

let float_to_int = TypeScheme.of_type (arrow TypeRepr.float TypeRepr.int)

let float_to_string = TypeScheme.of_type
  (arrow ~label:(TypeRepr.Optional "precision") TypeRepr.int (arrow TypeRepr.float TypeRepr.string))

let float_unop = TypeScheme.of_type (arrow TypeRepr.float TypeRepr.float)

let string_to_seq = TypeScheme.of_type (arrow TypeRepr.string (TypeRepr.seq TypeRepr.char))

let string_length = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.int)

let string_make = TypeScheme.of_type (arrow TypeRepr.int (arrow TypeRepr.char TypeRepr.string))

let string_get = TypeScheme.of_type (arrow TypeRepr.string (arrow TypeRepr.int TypeRepr.char))

let string_sub = TypeScheme.of_type
  (arrow TypeRepr.string (arrow TypeRepr.int (arrow TypeRepr.int TypeRepr.string)))

let string_concat = TypeScheme.of_type
  (arrow TypeRepr.string (arrow (TypeRepr.list TypeRepr.string) TypeRepr.string))

let string_split_on_char = TypeScheme.of_type
  (arrow TypeRepr.char (arrow TypeRepr.string (TypeRepr.list TypeRepr.string)))

let string_escaped = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.string)

let string_starts_with = TypeScheme.of_type
  (arrow ~label:(TypeRepr.Labelled "prefix") TypeRepr.string (arrow TypeRepr.string TypeRepr.bool))

let stdlib_buffer_type = named_path "Stdlib.Buffer.t"

let buffer_type = named_path "Buffer.t"

let stdlib_buffer_create = TypeScheme.of_type (arrow TypeRepr.int stdlib_buffer_type)

let stdlib_buffer_contents = TypeScheme.of_type (arrow stdlib_buffer_type TypeRepr.string)

let stdlib_buffer_add_char = TypeScheme.of_type
  (arrow stdlib_buffer_type (arrow TypeRepr.char TypeRepr.unit_))

let stdlib_buffer_add_string = TypeScheme.of_type
  (arrow stdlib_buffer_type (arrow TypeRepr.string TypeRepr.unit_))

let buffer_create = TypeScheme.of_type (arrow TypeRepr.int buffer_type)

let buffer_contents = TypeScheme.of_type (arrow buffer_type TypeRepr.string)

let buffer_add_char = TypeScheme.of_type (arrow buffer_type (arrow TypeRepr.char TypeRepr.unit_))

let buffer_add_string = TypeScheme.of_type (arrow buffer_type (arrow TypeRepr.string TypeRepr.unit_))

let exn_type = named_with_type_constructor_id
  ~type_constructor_id:BuiltinTypeConstructors.exn_type_constructor_id
  "exn"

let printexc_to_string = TypeScheme.of_type (arrow exn_type TypeRepr.string)

let stdlib_float_exports = [
  ("cbrt", float_unop);
  ("max", float_binop);
  ("min", float_binop);
  ("of_int", float_of_int);
  ("pow", float_binop);
  ("round", float_unop);
  ("to_int", float_to_int);
]

let float_exports = stdlib_float_exports @ [ ("to_string", float_to_string) ]

let stdlib_string_exports = [
  ("concat", string_concat);
  ("escaped", string_escaped);
  ("get", string_get);
  ("length", string_length);
  ("make", string_make);
  ("split_on_char", string_split_on_char);
  ("starts_with", string_starts_with);
  ("sub", string_sub);
  ("to_seq", string_to_seq);
]

let stdlib_list_exports = [
  ("filter_map", polymorphic_list_filter_map);
  ("for_all", polymorphic_list_for_all);
  ("map", polymorphic_list_map);
  ("of_seq", polymorphic_list_of_seq);
  ("rev", polymorphic_list_rev);
]

let stdlib_array_exports = [ ("length", polymorphic_array_length); ]

let stdlib_buffer_exports = [
  ("add_char", stdlib_buffer_add_char);
  ("add_string", stdlib_buffer_add_string);
  ("contents", stdlib_buffer_contents);
  ("create", stdlib_buffer_create);
]

let buffer_exports = [
  ("add_char", buffer_add_char);
  ("add_string", buffer_add_string);
  ("contents", buffer_contents);
  ("create", buffer_create);
]

let stdlib_printexc_exports = [ ("to_string", printexc_to_string); ]

let prefix_exports = fun module_name exports ->
  exports |> List.map (fun (name, scheme) -> (qualified_export_name module_name name, scheme))

let summaries = [
  (
    "Stdlib",
    prefix_exports "Array" stdlib_array_exports
    @ prefix_exports "Buffer" stdlib_buffer_exports
    @ prefix_exports "Float" stdlib_float_exports
    @ prefix_exports "List" stdlib_list_exports
    @ prefix_exports "Printexc" stdlib_printexc_exports
    @ prefix_exports "String" stdlib_string_exports,
    [ stdlib_buffer_t_decl; stdlib_seq_t_decl ]
  );
  ("Int", [ ("to_string", int_to_string); ("of_string", int_of_string) ], []);
  ("String", stdlib_string_exports, []);
  ("List", stdlib_list_exports, []);
  ("Array", stdlib_array_exports, []);
  ("Buffer", buffer_exports, [ buffer_t_decl ]);
  ("Float", float_exports, []);
  ("Printexc", stdlib_printexc_exports, []);
]
|> List.mapi
  (fun index (module_name, exports, type_decls) ->
    module_typings ~source_id:(SourceId.of_int ((-1_000) - index)) ~module_name ~type_decls exports)
