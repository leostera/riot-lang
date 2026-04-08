open Std
open Model

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

let named_path = fun path -> TypeRepr.named_path ~name:(IdentPath.of_string path) ~arguments:[]

let qualified_export_name = fun module_path name ->
  IdentPath.append_name (IdentPath.of_string module_path) name |> IdentPath.to_string

let type_decl = fun ~scope_path declaration -> { FileSummary.scope_path; declaration }

let module_typings = fun ~source_id ~module_name ?(type_decls = []) exports ->
  let file_summary = FileSummary.trusted ~source_id ~type_decls exports in
  let export_result = file_summary.export_result in
  let source_hash = ModuleTypings.synthetic_source_hash ~module_name ~export_result ~type_decls () in
  ModuleTypings.of_file_summary ~module_name ~source_hash file_summary

let abstract_type_decl = fun ~scope_path ~path ~type_name ?(param_ids = []) ?(param_variances = []) () ->
  type_decl ~scope_path
    {
      TypeDecl.type_constructor_id = TypeConstructorId.of_path (IdentPath.of_string path);
      type_name;
      param_ids;
      param_variances;
      constructors = [];
      labels = [];
      manifest = None;
    }

let alias_type_decl = fun ~scope_path ~path ~type_name ?(param_ids = []) ?(param_variances = []) manifest ->
  type_decl ~scope_path
    {
      TypeDecl.type_constructor_id = TypeConstructorId.of_path (IdentPath.of_string path);
      type_name;
      param_ids;
      param_variances;
      constructors = [];
      labels = [];
      manifest = Some (TypeDecl.Alias manifest);
    }

let constructor = fun id name scheme ->
  ({ TypeDecl.constructor_id = ConstructorId.of_int id; name; scheme }: TypeDecl.constructor)

let variant_type_decl = fun ~scope_path ~path ~type_name constructors ->
  type_decl ~scope_path
    {
      TypeDecl.type_constructor_id = TypeConstructorId.of_path (IdentPath.of_string path);
      type_name;
      param_ids = [];
      param_variances = [];
      constructors;
      labels = [];
      manifest = None;
    }

let variant_type_decl_with_params = fun ~scope_path ~path ~type_name ?(param_ids = []) ?(param_variances = []) constructors ->
  type_decl ~scope_path
    {
      TypeDecl.type_constructor_id = TypeConstructorId.of_path (IdentPath.of_string path);
      type_name;
      param_ids;
      param_variances;
      constructors;
      labels = [];
      manifest = None;
    }

let prefix_exports = fun module_path exports ->
  exports |> List.map (fun (name, scheme) -> (qualified_export_name module_path name, scheme))

let int_binop = monomorphic (arrow TypeRepr.int (arrow TypeRepr.int TypeRepr.int))

let int_compare = monomorphic (arrow TypeRepr.int (arrow TypeRepr.int TypeRepr.int))

let float_binop = monomorphic (arrow TypeRepr.float (arrow TypeRepr.float TypeRepr.float))

let float_unop = TypeScheme.of_type (arrow TypeRepr.float TypeRepr.float)

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

let polymorphic_array_of_list =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (TypeRepr.list element) (TypeRepr.array element))

let polymorphic_array_get =
  let element = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow (TypeRepr.array element) (arrow TypeRepr.int element))

let polymorphic_array_set =
  let element = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow (TypeRepr.array element) (arrow TypeRepr.int (arrow element TypeRepr.unit_)))

let polymorphic_array_make =
  let element = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow TypeRepr.int (arrow element (TypeRepr.array element)))

let polymorphic_array_init =
  let element = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow TypeRepr.int (arrow (arrow TypeRepr.int element) (TypeRepr.array element)))

let polymorphic_array_copy =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (TypeRepr.array element) (TypeRepr.array element))

let polymorphic_array_blit =
  let element = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow
      (TypeRepr.array element)
      (arrow TypeRepr.int (arrow (TypeRepr.array element) (arrow TypeRepr.int (arrow TypeRepr.int TypeRepr.unit_)))))

let exn_type = bare_named "exn"

let bytes_type = bare_named "bytes"

let stdlib_buffer_type = named_path "Stdlib.Buffer.t"

let buffer_type = named_path "Buffer.t"

let stdlib_uchar_type = named_path "Stdlib.Uchar.t"

let uchar_type = named_path "Uchar.t"

let stdlib_hashtbl_type key value =
  TypeRepr.named_path ~name:(IdentPath.of_string "Stdlib.Hashtbl.t") ~arguments:[ key; value ]

let hashtbl_type key value =
  TypeRepr.named_path ~name:(IdentPath.of_string "Hashtbl.t") ~arguments:[ key; value ]

let unix_file_descr_type = named_path "Unix.file_descr"

let unix_open_flag_type = named_path "Unix.open_flag"

let stdlib_sys_signal_behavior_type = named_path "Stdlib.Sys.signal_behavior"

let polymorphic_compare =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow element (arrow element TypeRepr.int))

let polymorphic_order =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow element (arrow element TypeRepr.bool))

let polymorphic_min_max =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow element (arrow element element))

let polymorphic_ignore =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow element TypeRepr.unit_)

let polymorphic_identity =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow element element)

let polymorphic_fst =
  let left = var 0 in
  let right = var 1 in
  TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow (TypeRepr.tuple [ left; right ]) left)

let polymorphic_snd =
  let left = var 0 in
  let right = var 1 in
  TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow (TypeRepr.tuple [ left; right ]) right)

let polymorphic_raise =
  let result = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow exn_type result)

let polymorphic_invalid_arg =
  let result = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow TypeRepr.string result)

let polymorphic_obj_magic =
  let source = var 0 in
  let target = var 1 in
  TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow source target)

let polymorphic_hashtbl_create =
  let key = var 0 in
  let value = var 1 in
  TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow TypeRepr.int (hashtbl_type key value))

let polymorphic_hashtbl_clear =
  let key = var 0 in
  let value = var 1 in
  TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow (hashtbl_type key value) TypeRepr.unit_)

let polymorphic_hashtbl_copy =
  let key = var 0 in
  let value = var 1 in
  TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow (hashtbl_type key value) (hashtbl_type key value))

let polymorphic_hashtbl_find =
  let key = var 0 in
  let value = var 1 in
  TypeScheme.of_explicit
    ~quantified:[ 0; 1 ]
    (arrow (hashtbl_type key value) (arrow key value))

let polymorphic_hashtbl_fold =
  let key = var 0 in
  let value = var 1 in
  let acc = var 2 in
  TypeScheme.of_explicit
    ~quantified:[ 0; 1; 2 ]
    (arrow
      (arrow key (arrow value (arrow acc acc)))
      (arrow (hashtbl_type key value) (arrow acc acc)))

let polymorphic_hashtbl_iter =
  let key = var 0 in
  let value = var 1 in
  TypeScheme.of_explicit
    ~quantified:[ 0; 1 ]
    (arrow
      (arrow key (arrow value TypeRepr.unit_))
      (arrow (hashtbl_type key value) TypeRepr.unit_))

let polymorphic_hashtbl_length =
  let key = var 0 in
  let value = var 1 in
  TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow (hashtbl_type key value) TypeRepr.int)

let polymorphic_hashtbl_mem =
  let key = var 0 in
  let value = var 1 in
  TypeScheme.of_explicit
    ~quantified:[ 0; 1 ]
    (arrow (hashtbl_type key value) (arrow key TypeRepr.bool))

let polymorphic_hashtbl_remove =
  let key = var 0 in
  let value = var 1 in
  TypeScheme.of_explicit
    ~quantified:[ 0; 1 ]
    (arrow (hashtbl_type key value) (arrow key TypeRepr.unit_))

let polymorphic_hashtbl_replace =
  let key = var 0 in
  let value = var 1 in
  TypeScheme.of_explicit
    ~quantified:[ 0; 1 ]
    (arrow (hashtbl_type key value) (arrow key (arrow value TypeRepr.unit_)))

let polymorphic_hash =
  let value = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow value TypeRepr.int)

let polymorphic_seeded_hash =
  let value = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow TypeRepr.int (arrow value TypeRepr.int))

let polymorphic_list_append =
  let element = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow (TypeRepr.list element) (arrow (TypeRepr.list element) (TypeRepr.list element)))

let polymorphic_pipe =
  let source = var 0 in
  let target = var 1 in
  TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow source (arrow (arrow source target) target))

let polymorphic_apply =
  let source = var 0 in
  let target = var 1 in
  TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow (arrow source target) (arrow source target))

let int_to_string = TypeScheme.of_type (arrow TypeRepr.int TypeRepr.string)

let int_of_string = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.int)

let float_of_int = TypeScheme.of_type (arrow TypeRepr.int TypeRepr.float)

let float_to_int = TypeScheme.of_type (arrow TypeRepr.float TypeRepr.int)

let float_to_string = TypeScheme.of_type
  (arrow
    ~label:(TypeRepr.Optional "precision")
    TypeRepr.int
    (arrow TypeRepr.float TypeRepr.string))

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

let polymorphic_seq_of_list =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (TypeRepr.list element) (TypeRepr.seq element))

let bytes_make = TypeScheme.of_type (arrow TypeRepr.int (arrow TypeRepr.char bytes_type))

let bytes_equal = TypeScheme.of_type (arrow bytes_type (arrow bytes_type TypeRepr.bool))

let bytes_compare = TypeScheme.of_type (arrow bytes_type (arrow bytes_type TypeRepr.int))

let bytes_of_string = TypeScheme.of_type (arrow TypeRepr.string bytes_type)

let bytes_to_string = TypeScheme.of_type (arrow bytes_type TypeRepr.string)

let bytes_copy = TypeScheme.of_type (arrow bytes_type bytes_type)

let bytes_length = TypeScheme.of_type (arrow bytes_type TypeRepr.int)

let bytes_get = TypeScheme.of_type (arrow bytes_type (arrow TypeRepr.int TypeRepr.char))

let bytes_set = TypeScheme.of_type
  (arrow bytes_type (arrow TypeRepr.int (arrow TypeRepr.char TypeRepr.unit_)))

let stdlib_buffer_create = TypeScheme.of_type (arrow TypeRepr.int stdlib_buffer_type)

let stdlib_buffer_contents = TypeScheme.of_type (arrow stdlib_buffer_type TypeRepr.string)

let stdlib_buffer_add_char = TypeScheme.of_type
  (arrow stdlib_buffer_type (arrow TypeRepr.char TypeRepr.unit_))

let stdlib_buffer_add_string = TypeScheme.of_type
  (arrow stdlib_buffer_type (arrow TypeRepr.string TypeRepr.unit_))

let stdlib_buffer_add_bytes = TypeScheme.of_type
  (arrow stdlib_buffer_type (arrow bytes_type TypeRepr.unit_))

let stdlib_buffer_add_utf_8_uchar = TypeScheme.of_type
  (arrow stdlib_buffer_type (arrow stdlib_uchar_type TypeRepr.unit_))

let buffer_create = TypeScheme.of_type (arrow TypeRepr.int buffer_type)

let buffer_contents = TypeScheme.of_type (arrow buffer_type TypeRepr.string)

let buffer_add_char = TypeScheme.of_type (arrow buffer_type (arrow TypeRepr.char TypeRepr.unit_))

let buffer_add_string = TypeScheme.of_type
  (arrow buffer_type (arrow TypeRepr.string TypeRepr.unit_))

let buffer_add_bytes = TypeScheme.of_type
  (arrow buffer_type (arrow bytes_type TypeRepr.unit_))

let buffer_add_utf_8_uchar = TypeScheme.of_type
  (arrow buffer_type (arrow uchar_type TypeRepr.unit_))

let printexc_to_string = TypeScheme.of_type (arrow exn_type TypeRepr.string)

let stdlib_bool_to_string = TypeScheme.of_type (arrow TypeRepr.bool TypeRepr.string)

let stdlib_char_code = TypeScheme.of_type (arrow TypeRepr.char TypeRepr.int)

let int32_type = bare_named "int32"

let int64_type = bare_named "int64"

let stdlib_int32_to_string = TypeScheme.of_type (arrow int32_type TypeRepr.string)

let stdlib_int64_to_string = TypeScheme.of_type (arrow int64_type TypeRepr.string)

let fun_protect =
  let result = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow
      ~label:(TypeRepr.Labelled "finally")
      (arrow TypeRepr.unit_ TypeRepr.unit_)
      (arrow (arrow TypeRepr.unit_ result) result))

let sys_getenv = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.string)

let sys_file_exists = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.bool)

let sys_is_directory = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.bool)

let sys_remove = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.unit_)

let sys_getcwd = TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.string)

let sys_chdir = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.unit_)

let sys_readdir =
  TypeScheme.of_type (arrow TypeRepr.string (TypeRepr.array TypeRepr.string))

let filename_get_temp_dir_name = TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.string)

let filename_temp_dir = TypeScheme.of_type
  (arrow
    ~label:(TypeRepr.Optional "temp_dir")
    TypeRepr.string
    (arrow TypeRepr.string (arrow TypeRepr.string TypeRepr.string)))

let sys_signal = TypeScheme.of_type
  (arrow TypeRepr.int (arrow stdlib_sys_signal_behavior_type stdlib_sys_signal_behavior_type))

let sys_set_signal = TypeScheme.of_type
  (arrow TypeRepr.int (arrow stdlib_sys_signal_behavior_type TypeRepr.unit_))

let sys_signal_constant = TypeScheme.of_type TypeRepr.int

let sys_runtime_variant = TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.string)

let sys_runtime_parameters = TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.string)

let sys_catch_break = TypeScheme.of_type (arrow TypeRepr.bool TypeRepr.unit_)

let sys_enable_runtime_warnings = TypeScheme.of_type (arrow TypeRepr.bool TypeRepr.unit_)

let sys_runtime_warnings_enabled = TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.bool)

let stdlib_type_id_t element =
  TypeRepr.named_path ~name:(IdentPath.of_string "Stdlib.Type.Id.t") ~arguments:[ element ]

let effect_perform =
  let value = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (TypeRepr.named_path ~name:(IdentPath.of_string "Stdlib.Effect.t") ~arguments:[ value ]) value)

let type_id_make =
  let value = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow TypeRepr.unit_ (stdlib_type_id_t value))

let type_id_uid =
  let value = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (stdlib_type_id_t value) TypeRepr.int)

let domain_type_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_string "Domain")
  ~path:"Stdlib.Domain.t"
  ~type_name:"t"
  ~param_ids:[ 0 ]
  ~param_variances:[ TypeDecl.Covariant ]
  ()

let domain_dls_key_type_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_string "Domain.DLS")
  ~path:"Stdlib.Domain.DLS.key"
  ~type_name:"key"
  ~param_ids:[ 0 ]
  ~param_variances:[ TypeDecl.Invariant ]
  ()

let domain_t element =
  TypeRepr.named_path ~name:(IdentPath.of_string "Stdlib.Domain.t") ~arguments:[ element ]

let domain_dls_key_type element =
  TypeRepr.named_path ~name:(IdentPath.of_string "Stdlib.Domain.DLS.key") ~arguments:[ element ]

let domain_spawn =
  let result = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (arrow TypeRepr.unit_ result) (domain_t result))

let domain_join =
  let result = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (domain_t result) result)

let domain_recommended_domain_count = TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.int)

let domain_dls_new_key =
  let value = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow (arrow TypeRepr.unit_ value) (domain_dls_key_type value))

let domain_dls_get =
  let value = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (domain_dls_key_type value) value)

let domain_dls_set =
  let value = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow (domain_dls_key_type value) (arrow value TypeRepr.unit_))

let stdlib_uchar_t_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_string "Uchar")
  ~path:"Stdlib.Uchar.t"
  ~type_name:"t"
  ()

let uchar_t_decl = abstract_type_decl
  ~scope_path:IdentPath.empty
  ~path:"Uchar.t"
  ~type_name:"t"
  ()

let stdlib_bytes_t_decl = alias_type_decl
  ~scope_path:(IdentPath.of_string "Bytes")
  ~path:"Stdlib.Bytes.t"
  ~type_name:"t"
  bytes_type

let bytes_t_decl = alias_type_decl
  ~scope_path:IdentPath.empty
  ~path:"Bytes.t"
  ~type_name:"t"
  bytes_type

let stdlib_seq_t_decl =
  let element = var 0 in
  alias_type_decl
    ~scope_path:(IdentPath.of_name "Seq")
    ~path:"Stdlib.Seq.t"
    ~type_name:"t"
    ~param_ids:[ 0 ]
    ~param_variances:[ TypeDecl.Covariant ]
    (TypeRepr.seq element)

let stdlib_seq_node_decl =
  let element = var 0 in
  let node_type =
    TypeRepr.named_path ~name:(IdentPath.of_string "Stdlib.Seq.node") ~arguments:[ element ]
  in
  variant_type_decl_with_params
    ~scope_path:(IdentPath.of_name "Seq")
    ~path:"Stdlib.Seq.node"
    ~type_name:"node"
    ~param_ids:[ 0 ]
    ~param_variances:[ TypeDecl.Covariant ]
    [
      constructor (-340) "Nil" (TypeScheme.of_explicit ~quantified:[ 0 ] node_type);
      constructor (-341) "Cons"
        (TypeScheme.of_explicit
          ~quantified:[ 0 ]
          (arrow element (arrow (TypeRepr.seq element) node_type)));
    ]

let stdlib_buffer_t_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_name "Buffer")
  ~path:"Stdlib.Buffer.t"
  ~type_name:"t"
  ()

let buffer_t_decl = abstract_type_decl
  ~scope_path:IdentPath.empty
  ~path:"Buffer.t"
  ~type_name:"t"
  ()

let stdlib_hashtbl_t_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_string "Hashtbl")
  ~path:"Stdlib.Hashtbl.t"
  ~type_name:"t"
  ~param_ids:[ 0; 1 ]
  ~param_variances:[ TypeDecl.Invariant; TypeDecl.Invariant ]
  ()

let hashtbl_t_decl = abstract_type_decl
  ~scope_path:IdentPath.empty
  ~path:"Hashtbl.t"
  ~type_name:"t"
  ~param_ids:[ 0; 1 ]
  ~param_variances:[ TypeDecl.Invariant; TypeDecl.Invariant ]
  ()

let stdlib_fpclass_decl =
  let fpclass_type = named_path "Stdlib.fpclass" in
  let ctor local_id name =
    constructor local_id name (TypeScheme.of_type fpclass_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Stdlib.fpclass"
    ~type_name:"fpclass"
    [
      ctor (-300) "FP_normal";
      ctor (-301) "FP_subnormal";
      ctor (-302) "FP_zero";
      ctor (-303) "FP_infinite";
      ctor (-304) "FP_nan";
    ]

let stdlib_sys_signal_behavior_decl =
  let ctor local_id name scheme = constructor local_id name scheme in
  variant_type_decl
    ~scope_path:(IdentPath.of_string "Sys")
    ~path:"Stdlib.Sys.signal_behavior"
    ~type_name:"signal_behavior"
    [
      ctor (-320) "Signal_default" (TypeScheme.of_type stdlib_sys_signal_behavior_type);
      ctor (-321) "Signal_ignore" (TypeScheme.of_type stdlib_sys_signal_behavior_type);
      ctor (-322) "Signal_handle"
        (TypeScheme.of_type
          (arrow
            (arrow TypeRepr.int TypeRepr.unit_)
            stdlib_sys_signal_behavior_type));
    ]

let stdlib_effect_t_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_string "Effect")
  ~path:"Stdlib.Effect.t"
  ~type_name:"t"
  ~param_ids:[ 0 ]
  ~param_variances:[ TypeDecl.Invariant ]
  ()

let stdlib_type_eq_decl =
  let left = var 0 in
  let equal_scheme =
    TypeScheme.of_explicit
      ~quantified:[ 0 ]
      (TypeRepr.named_path
        ~name:(IdentPath.of_string "Stdlib.Type.eq")
        ~arguments:[ left; left ])
  in
  variant_type_decl_with_params
    ~scope_path:(IdentPath.of_string "Type")
    ~path:"Stdlib.Type.eq"
    ~type_name:"eq"
    ~param_ids:[ 0; 1 ]
    ~param_variances:[ TypeDecl.Invariant; TypeDecl.Invariant ]
    [ constructor (-330) "Equal" equal_scheme ]

let stdlib_type_id_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_string "Type.Id")
  ~path:"Stdlib.Type.Id.t"
  ~type_name:"t"
  ~param_ids:[ 0 ]
  ~param_variances:[ TypeDecl.Invariant ]
  ()

let unix_file_descr_decl = abstract_type_decl
  ~scope_path:IdentPath.empty
  ~path:"Unix.file_descr"
  ~type_name:"file_descr"
  ()

let unix_open_flag_decl =
  let flag name id =
    constructor id name (TypeScheme.of_type unix_open_flag_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.open_flag"
    ~type_name:"open_flag"
    [
      flag "O_RDONLY" (-200);
      flag "O_WRONLY" (-201);
      flag "O_RDWR" (-202);
      flag "O_CREAT" (-203);
      flag "O_TRUNC" (-204);
      flag "O_APPEND" (-205);
      flag "O_EXCL" (-206);
    ]

let stdlib_array_exports = [
  ("blit", polymorphic_array_blit);
  ("copy", polymorphic_array_copy);
  ("get", polymorphic_array_get);
  ("init", polymorphic_array_init);
  ("length", polymorphic_array_length);
  ("make", polymorphic_array_make);
  ("of_list", polymorphic_array_of_list);
  ("set", polymorphic_array_set);
  ("to_list", TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (TypeRepr.array (var 0)) (TypeRepr.list (var 0))));
  ("unsafe_get", polymorphic_array_get);
  ("unsafe_set", polymorphic_array_set);
]

let stdlib_buffer_exports = [
  ("add_bytes", stdlib_buffer_add_bytes);
  ("add_char", stdlib_buffer_add_char);
  ("add_string", stdlib_buffer_add_string);
  ("add_utf_8_uchar", stdlib_buffer_add_utf_8_uchar);
  ("contents", stdlib_buffer_contents);
  ("create", stdlib_buffer_create);
]

let stdlib_float_exports = [
  ("cbrt", float_unop);
  ("max", float_binop);
  ("min", float_binop);
  ("of_int", float_of_int);
  ("pow", float_binop);
  ("round", float_unop);
  ("to_int", float_to_int);
  ("to_string", float_to_string);
]

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
  ("iter", TypeScheme.of_explicit ~quantified:[ 0 ] (arrow (arrow (var 0) TypeRepr.unit_) (arrow (TypeRepr.list (var 0)) TypeRepr.unit_)));
  ("map", polymorphic_list_map);
  ("of_seq", polymorphic_list_of_seq);
  ("rev", polymorphic_list_rev);
]

let stdlib_printexc_exports = [
  ("to_string", printexc_to_string);
]

let stdlib_seq_exports = [
  ("of_list", polymorphic_seq_of_list);
]

let stdlib_int_exports = [
  ("of_string", int_of_string);
  ("to_string", int_to_string);
]

let stdlib_bool_exports = [
  ("to_string", stdlib_bool_to_string);
]

let stdlib_char_exports = [
  ("code", stdlib_char_code);
]

let stdlib_int32_exports = [
  ("to_string", stdlib_int32_to_string);
]

let stdlib_int64_exports = [
  ("to_string", stdlib_int64_to_string);
]

let stdlib_bytes_exports = [
  ("compare", bytes_compare);
  ("copy", bytes_copy);
  ("equal", bytes_equal);
  ("get", bytes_get);
  ("length", bytes_length);
  ("make", bytes_make);
  ("of_string", bytes_of_string);
  ("set", bytes_set);
  ("to_string", bytes_to_string);
  ("unsafe_of_string", bytes_of_string);
]

let stdlib_fun_exports = [
  ("protect", fun_protect);
]

let stdlib_sys_exports = [
  ("Break", TypeScheme.of_type exn_type);
  ("argv", TypeScheme.of_type (TypeRepr.array TypeRepr.string));
  ("big_endian", TypeScheme.of_type TypeRepr.bool);
  ("chdir", sys_chdir);
  ("catch_break", sys_catch_break);
  ("cygwin", TypeScheme.of_type TypeRepr.bool);
  ("enable_runtime_warnings", sys_enable_runtime_warnings);
  ("executable_name", TypeScheme.of_type TypeRepr.string);
  ("file_exists", sys_file_exists);
  ("getcwd", sys_getcwd);
  ("getenv", sys_getenv);
  ("int_size", TypeScheme.of_type TypeRepr.int);
  ("is_directory", sys_is_directory);
  ("max_array_length", TypeScheme.of_type TypeRepr.int);
  ("max_floatarray_length", TypeScheme.of_type TypeRepr.int);
  ("max_string_length", TypeScheme.of_type TypeRepr.int);
  ("ocaml_version", TypeScheme.of_type TypeRepr.string);
  ("opaque_identity", polymorphic_identity);
  ("os_type", TypeScheme.of_type TypeRepr.string);
  ("readdir", sys_readdir);
  ("remove", sys_remove);
  ("runtime_parameters", sys_runtime_parameters);
  ("runtime_variant", sys_runtime_variant);
  ("runtime_warnings_enabled", sys_runtime_warnings_enabled);
  ("set_signal", sys_set_signal);
  ("sigabrt", sys_signal_constant);
  ("sigalrm", sys_signal_constant);
  ("sigbus", sys_signal_constant);
  ("sigchld", sys_signal_constant);
  ("sigcont", sys_signal_constant);
  ("sigfpe", sys_signal_constant);
  ("sighup", sys_signal_constant);
  ("sigill", sys_signal_constant);
  ("sigint", sys_signal_constant);
  ("sigkill", sys_signal_constant);
  ("sigpipe", sys_signal_constant);
  ("sigpoll", sys_signal_constant);
  ("sigprof", sys_signal_constant);
  ("sigquit", sys_signal_constant);
  ("sigsegv", sys_signal_constant);
  ("signal", sys_signal);
  ("sigstop", sys_signal_constant);
  ("sigsys", sys_signal_constant);
  ("sigterm", sys_signal_constant);
  ("sigtstp", sys_signal_constant);
  ("sigtrap", sys_signal_constant);
  ("sigttin", sys_signal_constant);
  ("sigttou", sys_signal_constant);
  ("sigurg", sys_signal_constant);
  ("sigusr1", sys_signal_constant);
  ("sigusr2", sys_signal_constant);
  ("sigvtalrm", sys_signal_constant);
  ("sigxcpu", sys_signal_constant);
  ("sigxfsz", sys_signal_constant);
  ("unix", TypeScheme.of_type TypeRepr.bool);
  ("win32", TypeScheme.of_type TypeRepr.bool);
  ("word_size", TypeScheme.of_type TypeRepr.int);
]

let stdlib_filename_exports = [
  ("get_temp_dir_name", filename_get_temp_dir_name);
  ("temp_dir", filename_temp_dir);
]

let stdlib_domain_exports = [
  ("join", domain_join);
  ("recommended_domain_count", domain_recommended_domain_count);
  ("spawn", domain_spawn);
]

let stdlib_domain_dls_exports = [
  ("get", domain_dls_get);
  ("new_key", domain_dls_new_key);
  ("set", domain_dls_set);
]

let stdlib_effect_exports = [
  ("perform", effect_perform);
]

let stdlib_type_id_exports = [
  ("make", type_id_make);
  ("uid", type_id_uid);
]

let bytes_exports = stdlib_bytes_exports

let hashtbl_exports = [
  ("clear", polymorphic_hashtbl_clear);
  ("copy", polymorphic_hashtbl_copy);
  ("create", polymorphic_hashtbl_create);
  ("find", polymorphic_hashtbl_find);
  ("fold", polymorphic_hashtbl_fold);
  ("hash", polymorphic_hash);
  ("iter", polymorphic_hashtbl_iter);
  ("length", polymorphic_hashtbl_length);
  ("mem", polymorphic_hashtbl_mem);
  ("remove", polymorphic_hashtbl_remove);
  ("replace", polymorphic_hashtbl_replace);
  ("seeded_hash", polymorphic_seeded_hash);
]

let obj_exports = [
  ("magic", polymorphic_obj_magic);
]

let unix_exports = [
  ("chdir", TypeScheme.of_type (arrow TypeRepr.string TypeRepr.unit_));
  ("clear_nonblock", TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.unit_));
  ("close", TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.unit_));
  ("environment", TypeScheme.of_type (arrow TypeRepr.unit_ (TypeRepr.array TypeRepr.string)));
  ("execv", TypeScheme.of_type
    (arrow TypeRepr.string (arrow (TypeRepr.array TypeRepr.string) TypeRepr.unit_)));
  ("getcwd", TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.string));
  ("getpid", TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.int));
  ("kill", TypeScheme.of_type (arrow TypeRepr.int (arrow TypeRepr.int TypeRepr.unit_)));
  ("isatty", TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.bool));
  ("openfile", TypeScheme.of_type
    (arrow TypeRepr.string (arrow (TypeRepr.list unix_open_flag_type) (arrow TypeRepr.int unix_file_descr_type))));
  ("pipe", TypeScheme.of_type (arrow TypeRepr.unit_ (TypeRepr.tuple [ unix_file_descr_type; unix_file_descr_type ])));
  ("putenv", TypeScheme.of_type (arrow TypeRepr.string (arrow TypeRepr.string TypeRepr.unit_)));
  ("sleepf", TypeScheme.of_type (arrow TypeRepr.float TypeRepr.unit_));
  ("set_close_on_exec", TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.unit_));
  ("set_nonblock", TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.unit_));
]

let stdlib_root_exports = [
  ("&&", TypeScheme.of_type (arrow TypeRepr.bool (arrow TypeRepr.bool TypeRepr.bool)));
  ("*", int_binop);
  ("*.", float_binop);
  ("+", int_binop);
  ("+.", float_binop);
  ("-", int_binop);
  ("-.", float_binop);
  ("/", int_binop);
  ("/.", float_binop);
  ("<", polymorphic_order);
  ("<=", polymorphic_order);
  ("<>", polymorphic_order);
  ("=", polymorphic_order);
  ("==", polymorphic_order);
  (">", polymorphic_order);
  (">=", polymorphic_order);
  ("@", polymorphic_list_append);
  ("@@", polymorphic_apply);
  ("^", TypeScheme.of_type (arrow TypeRepr.string (arrow TypeRepr.string TypeRepr.string)));
  ("asr", int_binop);
  ("Exit", TypeScheme.of_type exn_type);
  ("Not_found", TypeScheme.of_type exn_type);
  ("acos", float_unop);
  ("acosh", float_unop);
  ("abs", TypeScheme.of_type (arrow TypeRepr.int TypeRepr.int));
  ("asin", float_unop);
  ("asinh", float_unop);
  ("atan", float_unop);
  ("atan2", float_binop);
  ("atanh", float_unop);
  ("bool_of_string", TypeScheme.of_type (arrow TypeRepr.string TypeRepr.bool));
  ("bool_of_string_opt", TypeScheme.of_type (arrow TypeRepr.string (TypeRepr.option TypeRepr.bool)));
  ("ceil", float_unop);
  ("compare", polymorphic_compare);
  ("copysign", float_binop);
  ("cos", float_unop);
  ("cosh", float_unop);
  ("epsilon_float", TypeScheme.of_type TypeRepr.float);
  ("exit", TypeScheme.of_type (arrow TypeRepr.int TypeRepr.unit_));
  ("exp", float_unop);
  ("expm1", float_unop);
  ("float", TypeScheme.of_type (arrow TypeRepr.int TypeRepr.float));
  ("float_of_int", float_of_int);
  ("float_of_string", TypeScheme.of_type (arrow TypeRepr.string TypeRepr.float));
  ("float_of_string_opt", TypeScheme.of_type (arrow TypeRepr.string (TypeRepr.option TypeRepr.float)));
  ("floor", float_unop);
  ("frexp", TypeScheme.of_type (arrow TypeRepr.float (TypeRepr.tuple [ TypeRepr.float; TypeRepr.int ])));
  ("fst", polymorphic_fst);
  ("ignore", polymorphic_ignore);
  ("infinity", TypeScheme.of_type TypeRepr.float);
  ("int_of_float", TypeScheme.of_type (arrow TypeRepr.float TypeRepr.int));
  ("int_of_string", int_of_string);
  ("int_of_string_opt", TypeScheme.of_type (arrow TypeRepr.string (TypeRepr.option TypeRepr.int)));
  ("invalid_arg", polymorphic_invalid_arg);
  ("land", int_binop);
  ("ldexp", TypeScheme.of_type (arrow TypeRepr.float (arrow TypeRepr.int TypeRepr.float)));
  ("log", float_unop);
  ("log10", float_unop);
  ("log1p", float_unop);
  ("lnot", TypeScheme.of_type (arrow TypeRepr.int TypeRepr.int));
  ("lor", int_binop);
  ("lsl", int_binop);
  ("lsr", int_binop);
  ("lxor", int_binop);
  ("max", polymorphic_min_max);
  ("max_float", TypeScheme.of_type TypeRepr.float);
  ("max_int", TypeScheme.of_type TypeRepr.int);
  ("min", polymorphic_min_max);
  ("min_float", TypeScheme.of_type TypeRepr.float);
  ("min_int", TypeScheme.of_type TypeRepr.int);
  ("mod_float", float_binop);
  ("mod", int_binop);
  ("modf", TypeScheme.of_type (arrow TypeRepr.float (TypeRepr.tuple [ TypeRepr.float; TypeRepr.float ])));
  ("nan", TypeScheme.of_type TypeRepr.float);
  ("neg_infinity", TypeScheme.of_type TypeRepr.float);
  ("not", TypeScheme.of_type (arrow TypeRepr.bool TypeRepr.bool));
  ("||", TypeScheme.of_type (arrow TypeRepr.bool (arrow TypeRepr.bool TypeRepr.bool)));
  ("pred", TypeScheme.of_type (arrow TypeRepr.int TypeRepr.int));
  ("raise", polymorphic_raise);
  ("raise_notrace", polymorphic_raise);
  ("sin", float_unop);
  ("sinh", float_unop);
  ("snd", polymorphic_snd);
  ("sqrt", float_unop);
  ("string_of_bool", TypeScheme.of_type (arrow TypeRepr.bool TypeRepr.string));
  ("string_of_float", TypeScheme.of_type (arrow TypeRepr.float TypeRepr.string));
  ("string_of_int", int_to_string);
  ("succ", TypeScheme.of_type (arrow TypeRepr.int TypeRepr.int));
  ("tan", float_unop);
  ("tanh", float_unop);
  ("truncate", TypeScheme.of_type (arrow TypeRepr.float TypeRepr.int));
  ("|>", polymorphic_pipe);
  ("~+", TypeScheme.of_type (arrow TypeRepr.int TypeRepr.int));
  ("~+.", float_unop);
  ("~-", TypeScheme.of_type (arrow TypeRepr.int TypeRepr.int));
  ("~-.", float_unop);
]

let summaries = [
  (
    "Stdlib",
    stdlib_root_exports
    @ prefix_exports "Array" stdlib_array_exports
    @ prefix_exports "Bool" stdlib_bool_exports
    @ prefix_exports "Buffer" stdlib_buffer_exports
    @ prefix_exports "Bytes" stdlib_bytes_exports
    @ prefix_exports "Char" stdlib_char_exports
    @ prefix_exports "Domain" stdlib_domain_exports
    @ prefix_exports "Domain.DLS" stdlib_domain_dls_exports
    @ prefix_exports "Effect" stdlib_effect_exports
    @ prefix_exports "Filename" stdlib_filename_exports
    @ prefix_exports "Float" stdlib_float_exports
    @ prefix_exports "Fun" stdlib_fun_exports
    @ prefix_exports "Hashtbl" hashtbl_exports
    @ prefix_exports "Int" stdlib_int_exports
    @ prefix_exports "Int32" stdlib_int32_exports
    @ prefix_exports "Int64" stdlib_int64_exports
    @ prefix_exports "List" stdlib_list_exports
    @ prefix_exports "Seq" stdlib_seq_exports
    @ prefix_exports "Obj" obj_exports
    @ prefix_exports "Printexc" stdlib_printexc_exports
    @ prefix_exports "String" stdlib_string_exports
    @ prefix_exports "Sys" stdlib_sys_exports
    @ prefix_exports "Type.Id" stdlib_type_id_exports,
    [
      domain_dls_key_type_decl;
      domain_type_decl;
      stdlib_effect_t_decl;
      stdlib_buffer_t_decl;
      stdlib_bytes_t_decl;
      stdlib_fpclass_decl;
      stdlib_hashtbl_t_decl;
      stdlib_seq_node_decl;
      stdlib_seq_t_decl;
      stdlib_sys_signal_behavior_decl;
      stdlib_type_eq_decl;
      stdlib_type_id_decl;
      stdlib_uchar_t_decl;
    ]
  );
  ("Array", stdlib_array_exports, []);
  ("Buffer", [
    ("add_bytes", buffer_add_bytes);
    ("add_char", buffer_add_char);
    ("add_string", buffer_add_string);
    ("add_utf_8_uchar", buffer_add_utf_8_uchar);
    ("contents", buffer_contents);
    ("create", buffer_create);
  ], [ buffer_t_decl ]);
  ("Bytes", bytes_exports, [ bytes_t_decl ]);
  ("Float", stdlib_float_exports, []);
  ("Hashtbl", hashtbl_exports, [ hashtbl_t_decl ]);
  ("Int", stdlib_int_exports, []);
  ("List", stdlib_list_exports, []);
  ("Obj", obj_exports, []);
  ("Printexc", stdlib_printexc_exports, []);
  ("String", stdlib_string_exports, []);
  ("Unix", unix_exports, [ unix_file_descr_decl; unix_open_flag_decl ]);
]
|> List.mapi
  (fun index (module_name, exports, type_decls) ->
    module_typings ~source_id:(SourceId.of_int ((-1_000) - index)) ~module_name ~type_decls exports)
