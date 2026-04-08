open Std
open Model

let ocaml_stdlib_module_source_id_base = -1_000

let ocaml_stdlib_constructor_id_start = -10_000

let ocaml_stdlib_label_id_start = -20_000

let make_next_id = fun start ->
  let next = ref start in
  fun () ->
    let id = !next in
    let () = next := id - 1 in
    id

let next_constructor_id = make_next_id ocaml_stdlib_constructor_id_start

let next_label_id = make_next_id ocaml_stdlib_label_id_start

let monomorphic = fun ty -> TypeScheme.of_type ty

let var = fun id -> TypeRepr.make_var id

let arrow = fun ?(label = TypeRepr.Nolabel) lhs rhs -> TypeRepr.arrow ~label ~lhs ~rhs

let named_with_type_constructor_id = fun ~type_constructor_id name ->
  let path = IdentPath.of_name name in
  let head = TypeRepr.named_head ~type_constructor_id ~name:path in
  TypeRepr.named ~head ~arguments:[]

let bare_named = fun name ->
  let path = IdentPath.of_name name in
  let head = TypeRepr.named_head ~type_constructor_id:(TypeConstructorId.of_path path) ~name:path in
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

let constructor = fun name scheme ->
  ({
    TypeDecl.constructor_id = ConstructorId.of_int (next_constructor_id ());
    name;
    scheme;
    inline_record_labels = None;
  }: TypeDecl.constructor)

let label = fun ?(mutable_ = false) name field_type ->
  ({ TypeDecl.label_id = LabelId.of_int (next_label_id ()); name; field_type; mutable_ }: TypeDecl.label)

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

let record_type_decl = fun ~scope_path ~path ~type_name labels ->
  type_decl ~scope_path
    {
      TypeDecl.type_constructor_id = TypeConstructorId.of_path (IdentPath.of_string path);
      type_name;
      param_ids = [];
      param_variances = [];
      constructors = [];
      labels;
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

let exn_type = named_with_type_constructor_id
  ~type_constructor_id:BuiltinTypeConstructors.exn_type_constructor_id
  "exn"

let bytes_type = bare_named "bytes"

let stdlib_buffer_type = named_path "Stdlib.Buffer.t"

let buffer_type = named_path "Buffer.t"

let stdlib_uchar_type = named_path "Stdlib.Uchar.t"

let uchar_type = named_path "Uchar.t"

let stdlib_uchar_utf_decode_type = named_path "Stdlib.Uchar.utf_decode"

let nativeint_type = bare_named "nativeint"

let stdlib_hashtbl_type key value =
  TypeRepr.named_path ~name:(IdentPath.of_string "Stdlib.Hashtbl.t") ~arguments:[ key; value ]

let hashtbl_type key value =
  TypeRepr.named_path ~name:(IdentPath.of_string "Hashtbl.t") ~arguments:[ key; value ]

let unix_file_descr_type = named_path "Unix.file_descr"

let unix_open_flag_type = named_path "Unix.open_flag"

let unix_error_type = named_path "Unix.error"

let unix_file_kind_type = named_path "Unix.file_kind"

let unix_stats_type = named_path "Unix.stats"

let unix_seek_command_type = named_path "Unix.seek_command"

let unix_lock_command_type = named_path "Unix.lock_command"

let unix_process_status_type = named_path "Unix.process_status"

let unix_wait_flag_type = named_path "Unix.wait_flag"

let unix_socket_domain_type = named_path "Unix.socket_domain"

let unix_socket_type_type = named_path "Unix.socket_type"

let unix_inet_addr_type = named_path "Unix.inet_addr"

let unix_sockaddr_type = named_path "Unix.sockaddr"

let unix_addr_info_type = named_path "Unix.addr_info"

let unix_terminal_io_type = named_path "Unix.terminal_io"

let unix_setattr_when_type = named_path "Unix.setattr_when"

let unix_socket_bool_option_type = named_path "Unix.socket_bool_option"

let unix_msg_flag_type = named_path "Unix.msg_flag"

let unix_getaddrinfo_option_type = named_path "Unix.getaddrinfo_option"

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

let polymorphic_exit =
  let result = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow TypeRepr.int result)

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

let int32_value = TypeScheme.of_type int32_type

let int64_value = TypeScheme.of_type int64_type

let int32_unop = TypeScheme.of_type (arrow int32_type int32_type)

let int64_unop = TypeScheme.of_type (arrow int64_type int64_type)

let int32_binop = TypeScheme.of_type (arrow int32_type (arrow int32_type int32_type))

let int64_binop = TypeScheme.of_type (arrow int64_type (arrow int64_type int64_type))

let int32_compare = TypeScheme.of_type (arrow int32_type (arrow int32_type TypeRepr.int))

let int64_compare = TypeScheme.of_type (arrow int64_type (arrow int64_type TypeRepr.int))

let int32_equal = TypeScheme.of_type (arrow int32_type (arrow int32_type TypeRepr.bool))

let int64_equal = TypeScheme.of_type (arrow int64_type (arrow int64_type TypeRepr.bool))

let stdlib_int32_to_string = TypeScheme.of_type (arrow int32_type TypeRepr.string)

let stdlib_int64_to_string = TypeScheme.of_type (arrow int64_type TypeRepr.string)

let stdlib_int32_of_int = TypeScheme.of_type (arrow TypeRepr.int int32_type)

let stdlib_int64_of_int = TypeScheme.of_type (arrow TypeRepr.int int64_type)

let stdlib_int32_to_int = TypeScheme.of_type (arrow int32_type TypeRepr.int)

let stdlib_int64_to_int = TypeScheme.of_type (arrow int64_type TypeRepr.int)

let stdlib_int32_unsigned_to_int = TypeScheme.of_type (arrow int32_type (TypeRepr.option TypeRepr.int))

let stdlib_int64_unsigned_to_int = TypeScheme.of_type (arrow int64_type (TypeRepr.option TypeRepr.int))

let stdlib_int32_of_float = TypeScheme.of_type (arrow TypeRepr.float int32_type)

let stdlib_int64_of_float = TypeScheme.of_type (arrow TypeRepr.float int64_type)

let stdlib_int32_to_float = TypeScheme.of_type (arrow int32_type TypeRepr.float)

let stdlib_int64_to_float = TypeScheme.of_type (arrow int64_type TypeRepr.float)

let stdlib_int32_of_string = TypeScheme.of_type (arrow TypeRepr.string int32_type)

let stdlib_int64_of_string = TypeScheme.of_type (arrow TypeRepr.string int64_type)

let stdlib_int32_of_string_opt = TypeScheme.of_type (arrow TypeRepr.string (TypeRepr.option int32_type))

let stdlib_int64_of_string_opt = TypeScheme.of_type (arrow TypeRepr.string (TypeRepr.option int64_type))

let stdlib_int32_bits_of_float = TypeScheme.of_type (arrow TypeRepr.float int32_type)

let stdlib_int64_bits_of_float = TypeScheme.of_type (arrow TypeRepr.float int64_type)

let stdlib_int32_float_of_bits = TypeScheme.of_type (arrow int32_type TypeRepr.float)

let stdlib_int64_float_of_bits = TypeScheme.of_type (arrow int64_type TypeRepr.float)

let stdlib_int64_of_int32 = TypeScheme.of_type (arrow int32_type int64_type)

let stdlib_int64_to_int32 = TypeScheme.of_type (arrow int64_type int32_type)

let stdlib_int64_of_nativeint = TypeScheme.of_type (arrow nativeint_type int64_type)

let stdlib_int64_to_nativeint = TypeScheme.of_type (arrow int64_type nativeint_type)

let stdlib_int32_seeded_hash = TypeScheme.of_type (arrow TypeRepr.int (arrow int32_type TypeRepr.int))

let stdlib_int64_seeded_hash = TypeScheme.of_type (arrow TypeRepr.int (arrow int64_type TypeRepr.int))

let stdlib_int32_hash = TypeScheme.of_type (arrow int32_type TypeRepr.int)

let stdlib_int64_hash = TypeScheme.of_type (arrow int64_type TypeRepr.int)

let stdlib_uchar_value = TypeScheme.of_type stdlib_uchar_type

let stdlib_uchar_pred_succ = TypeScheme.of_type (arrow stdlib_uchar_type stdlib_uchar_type)

let stdlib_uchar_of_int = TypeScheme.of_type (arrow TypeRepr.int stdlib_uchar_type)

let stdlib_uchar_to_int = TypeScheme.of_type (arrow stdlib_uchar_type TypeRepr.int)

let stdlib_uchar_is_valid = TypeScheme.of_type (arrow TypeRepr.int TypeRepr.bool)

let stdlib_uchar_is_char = TypeScheme.of_type (arrow stdlib_uchar_type TypeRepr.bool)

let stdlib_uchar_of_char = TypeScheme.of_type (arrow TypeRepr.char stdlib_uchar_type)

let stdlib_uchar_to_char = TypeScheme.of_type (arrow stdlib_uchar_type TypeRepr.char)

let stdlib_uchar_equal = TypeScheme.of_type
  (arrow stdlib_uchar_type (arrow stdlib_uchar_type TypeRepr.bool))

let stdlib_uchar_compare = TypeScheme.of_type
  (arrow stdlib_uchar_type (arrow stdlib_uchar_type TypeRepr.int))

let stdlib_uchar_seeded_hash = TypeScheme.of_type
  (arrow TypeRepr.int (arrow stdlib_uchar_type TypeRepr.int))

let stdlib_uchar_hash = TypeScheme.of_type (arrow stdlib_uchar_type TypeRepr.int)

let stdlib_uchar_utf_decode_is_valid = TypeScheme.of_type
  (arrow stdlib_uchar_utf_decode_type TypeRepr.bool)

let stdlib_uchar_utf_decode_uchar = TypeScheme.of_type
  (arrow stdlib_uchar_utf_decode_type stdlib_uchar_type)

let stdlib_uchar_utf_decode_length = TypeScheme.of_type
  (arrow stdlib_uchar_utf_decode_type TypeRepr.int)

let stdlib_uchar_utf_decode = TypeScheme.of_type
  (arrow TypeRepr.int (arrow stdlib_uchar_type stdlib_uchar_utf_decode_type))

let stdlib_uchar_utf_decode_invalid = TypeScheme.of_type
  (arrow TypeRepr.int stdlib_uchar_utf_decode_type)

let stdlib_uchar_utf_8_byte_length = TypeScheme.of_type
  (arrow stdlib_uchar_type TypeRepr.int)

let stdlib_uchar_utf_16_byte_length = TypeScheme.of_type
  (arrow stdlib_uchar_type TypeRepr.int)

let stdlib_random_state_type = named_path "Stdlib.Random.State.t"

let stdlib_random_init = TypeScheme.of_type (arrow TypeRepr.int TypeRepr.unit_)

let stdlib_random_full_init = TypeScheme.of_type
  (arrow (TypeRepr.array TypeRepr.int) TypeRepr.unit_)

let stdlib_random_self_init = TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.unit_)

let stdlib_random_bits = TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.int)

let stdlib_random_int = TypeScheme.of_type (arrow TypeRepr.int TypeRepr.int)

let stdlib_random_full_int = TypeScheme.of_type (arrow TypeRepr.int TypeRepr.int)

let stdlib_random_int_in_range = TypeScheme.of_type
  (arrow
    ~label:(TypeRepr.Labelled "min")
    TypeRepr.int
    (arrow ~label:(TypeRepr.Labelled "max") TypeRepr.int TypeRepr.int))

let stdlib_random_int32 = TypeScheme.of_type (arrow int32_type int32_type)

let stdlib_random_int32_in_range = TypeScheme.of_type
  (arrow
    ~label:(TypeRepr.Labelled "min")
    int32_type
    (arrow ~label:(TypeRepr.Labelled "max") int32_type int32_type))

let stdlib_random_int64 = TypeScheme.of_type (arrow int64_type int64_type)

let stdlib_random_int64_in_range = TypeScheme.of_type
  (arrow
    ~label:(TypeRepr.Labelled "min")
    int64_type
    (arrow ~label:(TypeRepr.Labelled "max") int64_type int64_type))

let stdlib_random_float = TypeScheme.of_type (arrow TypeRepr.float TypeRepr.float)

let stdlib_random_bool = TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.bool)

let stdlib_random_bits32 = TypeScheme.of_type (arrow TypeRepr.unit_ int32_type)

let stdlib_random_bits64 = TypeScheme.of_type (arrow TypeRepr.unit_ int64_type)

let stdlib_random_get_state = TypeScheme.of_type (arrow TypeRepr.unit_ stdlib_random_state_type)

let stdlib_random_set_state = TypeScheme.of_type
  (arrow stdlib_random_state_type TypeRepr.unit_)

let stdlib_random_split = TypeScheme.of_type (arrow TypeRepr.unit_ stdlib_random_state_type)

let stdlib_random_state_make = TypeScheme.of_type
  (arrow (TypeRepr.array TypeRepr.int) stdlib_random_state_type)

let stdlib_random_state_make_self_init = TypeScheme.of_type
  (arrow TypeRepr.unit_ stdlib_random_state_type)

let stdlib_random_state_copy = TypeScheme.of_type
  (arrow stdlib_random_state_type stdlib_random_state_type)

let stdlib_random_state_bits = TypeScheme.of_type
  (arrow stdlib_random_state_type TypeRepr.int)

let stdlib_random_state_int = TypeScheme.of_type
  (arrow stdlib_random_state_type (arrow TypeRepr.int TypeRepr.int))

let stdlib_random_state_full_int = TypeScheme.of_type
  (arrow stdlib_random_state_type (arrow TypeRepr.int TypeRepr.int))

let stdlib_random_state_int_in_range = TypeScheme.of_type
  (arrow
    stdlib_random_state_type
    (arrow
      ~label:(TypeRepr.Labelled "min")
      TypeRepr.int
      (arrow ~label:(TypeRepr.Labelled "max") TypeRepr.int TypeRepr.int)))

let stdlib_random_state_int32 = TypeScheme.of_type
  (arrow stdlib_random_state_type (arrow int32_type int32_type))

let stdlib_random_state_int32_in_range = TypeScheme.of_type
  (arrow
    stdlib_random_state_type
    (arrow
      ~label:(TypeRepr.Labelled "min")
      int32_type
      (arrow ~label:(TypeRepr.Labelled "max") int32_type int32_type)))

let stdlib_random_state_int64 = TypeScheme.of_type
  (arrow stdlib_random_state_type (arrow int64_type int64_type))

let stdlib_random_state_int64_in_range = TypeScheme.of_type
  (arrow
    stdlib_random_state_type
    (arrow
      ~label:(TypeRepr.Labelled "min")
      int64_type
      (arrow ~label:(TypeRepr.Labelled "max") int64_type int64_type)))

let stdlib_random_state_float = TypeScheme.of_type
  (arrow stdlib_random_state_type (arrow TypeRepr.float TypeRepr.float))

let stdlib_random_state_bool = TypeScheme.of_type
  (arrow stdlib_random_state_type TypeRepr.bool)

let stdlib_random_state_bits32 = TypeScheme.of_type
  (arrow stdlib_random_state_type int32_type)

let stdlib_random_state_bits64 = TypeScheme.of_type
  (arrow stdlib_random_state_type int64_type)

let stdlib_random_state_split = TypeScheme.of_type
  (arrow stdlib_random_state_type stdlib_random_state_type)

let stdlib_random_state_to_binary_string = TypeScheme.of_type
  (arrow stdlib_random_state_type TypeRepr.string)

let stdlib_random_state_of_binary_string = TypeScheme.of_type
  (arrow TypeRepr.string stdlib_random_state_type)

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

let unix_error_message = TypeScheme.of_type (arrow unix_error_type TypeRepr.string)

let unix_inet_addr_of_string = TypeScheme.of_type (arrow TypeRepr.string unix_inet_addr_type)

let unix_string_of_inet_addr = TypeScheme.of_type (arrow unix_inet_addr_type TypeRepr.string)

let unix_socket = TypeScheme.of_type
  (arrow
    ~label:(TypeRepr.Optional "cloexec")
    TypeRepr.bool
    (arrow unix_socket_domain_type (arrow unix_socket_type_type (arrow TypeRepr.int unix_file_descr_type))))

let unix_bind = TypeScheme.of_type (arrow unix_file_descr_type (arrow unix_sockaddr_type TypeRepr.unit_))

let unix_listen = TypeScheme.of_type (arrow unix_file_descr_type (arrow TypeRepr.int TypeRepr.unit_))

let unix_connect = TypeScheme.of_type (arrow unix_file_descr_type (arrow unix_sockaddr_type TypeRepr.unit_))

let unix_accept = TypeScheme.of_type
  (arrow
    ~label:(TypeRepr.Optional "cloexec")
    TypeRepr.bool
    (arrow unix_file_descr_type (TypeRepr.tuple [ unix_file_descr_type; unix_sockaddr_type ])))

let unix_getsockname = TypeScheme.of_type (arrow unix_file_descr_type unix_sockaddr_type)

let unix_setsockopt = TypeScheme.of_type
  (arrow unix_file_descr_type (arrow unix_socket_bool_option_type (arrow TypeRepr.bool TypeRepr.unit_)))

let unix_recv = TypeScheme.of_type
  (arrow unix_file_descr_type
    (arrow bytes_type
      (arrow TypeRepr.int (arrow TypeRepr.int (arrow (TypeRepr.list unix_msg_flag_type) TypeRepr.int)))))

let unix_recvfrom = TypeScheme.of_type
  (arrow unix_file_descr_type
    (arrow bytes_type
      (arrow TypeRepr.int
        (arrow TypeRepr.int
          (arrow
            (TypeRepr.list unix_msg_flag_type)
            (TypeRepr.tuple [ TypeRepr.int; unix_sockaddr_type ]))))))

let unix_send = TypeScheme.of_type
  (arrow unix_file_descr_type
    (arrow bytes_type
      (arrow TypeRepr.int (arrow TypeRepr.int (arrow (TypeRepr.list unix_msg_flag_type) TypeRepr.int)))))

let unix_sendto = TypeScheme.of_type
  (arrow unix_file_descr_type
    (arrow bytes_type
      (arrow TypeRepr.int
        (arrow TypeRepr.int
          (arrow (TypeRepr.list unix_msg_flag_type) (arrow unix_sockaddr_type TypeRepr.int))))))

let unix_read = TypeScheme.of_type
  (arrow unix_file_descr_type (arrow bytes_type (arrow TypeRepr.int (arrow TypeRepr.int TypeRepr.int))))

let unix_mkdir = TypeScheme.of_type (arrow TypeRepr.string (arrow TypeRepr.int TypeRepr.unit_))

let unix_stat = TypeScheme.of_type (arrow TypeRepr.string unix_stats_type)

let unix_chmod = TypeScheme.of_type (arrow TypeRepr.string (arrow TypeRepr.int TypeRepr.unit_))

let unix_symlink = TypeScheme.of_type (arrow TypeRepr.string (arrow TypeRepr.string TypeRepr.unit_))

let unix_rmdir = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.unit_)

let unix_realpath = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.string)

let unix_link = TypeScheme.of_type (arrow TypeRepr.string (arrow TypeRepr.string TypeRepr.unit_))

let unix_rename = TypeScheme.of_type (arrow TypeRepr.string (arrow TypeRepr.string TypeRepr.unit_))

let unix_readlink = TypeScheme.of_type (arrow TypeRepr.string TypeRepr.string)

let unix_fstat = TypeScheme.of_type (arrow unix_file_descr_type unix_stats_type)

let unix_lstat = TypeScheme.of_type (arrow TypeRepr.string unix_stats_type)

let unix_lseek = TypeScheme.of_type
  (arrow unix_file_descr_type (arrow TypeRepr.int (arrow unix_seek_command_type TypeRepr.int)))

let unix_ftruncate = TypeScheme.of_type
  (arrow unix_file_descr_type (arrow TypeRepr.int TypeRepr.unit_))

let unix_fchmod = TypeScheme.of_type (arrow unix_file_descr_type (arrow TypeRepr.int TypeRepr.unit_))

let unix_fsync = TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.unit_)

let unix_dup = TypeScheme.of_type (arrow unix_file_descr_type unix_file_descr_type)

let unix_lockf = TypeScheme.of_type
  (arrow unix_file_descr_type (arrow unix_lock_command_type (arrow TypeRepr.int TypeRepr.unit_)))

let unix_create_process_env = TypeScheme.of_type
  (arrow TypeRepr.string
    (arrow
      (TypeRepr.array TypeRepr.string)
      (arrow
        (TypeRepr.array TypeRepr.string)
        (arrow unix_file_descr_type (arrow unix_file_descr_type (arrow unix_file_descr_type TypeRepr.int))))))

let unix_waitpid = TypeScheme.of_type
  (arrow (TypeRepr.list unix_wait_flag_type) (arrow TypeRepr.int (TypeRepr.tuple [ TypeRepr.int; unix_process_status_type ])))

let unix_tcgetattr = TypeScheme.of_type (arrow unix_file_descr_type unix_terminal_io_type)

let unix_tcsetattr = TypeScheme.of_type
  (arrow unix_file_descr_type (arrow unix_setattr_when_type (arrow unix_terminal_io_type TypeRepr.unit_)))

let unix_getaddrinfo = TypeScheme.of_type
  (arrow
    TypeRepr.string
    (arrow TypeRepr.string (arrow (TypeRepr.list unix_getaddrinfo_option_type) (TypeRepr.list unix_addr_info_type))))

let unixlabels_read = TypeScheme.of_type
  (arrow unix_file_descr_type
    (arrow
      ~label:(TypeRepr.Labelled "buf")
      bytes_type
      (arrow ~label:(TypeRepr.Labelled "pos") TypeRepr.int (arrow ~label:(TypeRepr.Labelled "len") TypeRepr.int TypeRepr.int))))

let unixlabels_write = TypeScheme.of_type
  (arrow unix_file_descr_type
    (arrow
      ~label:(TypeRepr.Labelled "buf")
      bytes_type
      (arrow ~label:(TypeRepr.Labelled "pos") TypeRepr.int (arrow ~label:(TypeRepr.Labelled "len") TypeRepr.int TypeRepr.int))))

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
    (arrow
      ~label:(TypeRepr.Optional "split_from_parent")
      (arrow value value)
      (arrow (arrow TypeRepr.unit_ value) (domain_dls_key_type value)))

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

let stdlib_uchar_utf_decode_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_string "Uchar")
  ~path:"Stdlib.Uchar.utf_decode"
  ~type_name:"utf_decode"
  ()

let uchar_t_decl = abstract_type_decl
  ~scope_path:IdentPath.empty
  ~path:"Uchar.t"
  ~type_name:"t"
  ()

let stdlib_int32_t_decl = alias_type_decl
  ~scope_path:(IdentPath.of_name "Int32")
  ~path:"Stdlib.Int32.t"
  ~type_name:"t"
  int32_type

let stdlib_int64_t_decl = alias_type_decl
  ~scope_path:(IdentPath.of_name "Int64")
  ~path:"Stdlib.Int64.t"
  ~type_name:"t"
  int64_type

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
      constructor "Nil" (TypeScheme.of_explicit ~quantified:[ 0 ] node_type);
      constructor "Cons"
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
  let ctor name =
    constructor name (TypeScheme.of_type fpclass_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Stdlib.fpclass"
    ~type_name:"fpclass"
    [
      ctor "FP_normal";
      ctor "FP_subnormal";
      ctor "FP_zero";
      ctor "FP_infinite";
      ctor "FP_nan";
    ]

let stdlib_sys_signal_behavior_decl =
  let ctor name scheme = constructor name scheme in
  variant_type_decl
    ~scope_path:(IdentPath.of_string "Sys")
    ~path:"Stdlib.Sys.signal_behavior"
    ~type_name:"signal_behavior"
    [
      ctor "Signal_default" (TypeScheme.of_type stdlib_sys_signal_behavior_type);
      ctor "Signal_ignore" (TypeScheme.of_type stdlib_sys_signal_behavior_type);
      ctor "Signal_handle"
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
    [ constructor "Equal" equal_scheme ]

let stdlib_type_id_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_string "Type.Id")
  ~path:"Stdlib.Type.Id.t"
  ~type_name:"t"
  ~param_ids:[ 0 ]
  ~param_variances:[ TypeDecl.Invariant ]
  ()

let stdlib_random_state_t_decl = abstract_type_decl
  ~scope_path:(IdentPath.of_string "Random.State")
  ~path:"Stdlib.Random.State.t"
  ~type_name:"t"
  ()

let unix_file_descr_decl = abstract_type_decl
  ~scope_path:IdentPath.empty
  ~path:"Unix.file_descr"
  ~type_name:"file_descr"
  ()

let unix_open_flag_decl =
  let flag name =
    constructor name (TypeScheme.of_type unix_open_flag_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.open_flag"
    ~type_name:"open_flag"
    [
      flag "O_RDONLY";
      flag "O_WRONLY";
      flag "O_RDWR";
      flag "O_CREAT";
      flag "O_TRUNC";
      flag "O_APPEND";
      flag "O_EXCL";
    ]

let unix_error_decl =
  let err name =
    constructor name (TypeScheme.of_type unix_error_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.error"
    ~type_name:"error"
    [
      err "E2BIG";
      err "EACCES";
      err "EADDRINUSE";
      err "EADDRNOTAVAIL";
      err "EAFNOSUPPORT";
      err "EAGAIN";
      err "EALREADY";
      err "EBADF";
      err "EBUSY";
      err "ECHILD";
      err "ECONNABORTED";
      err "ECONNREFUSED";
      err "ECONNRESET";
      err "EDEADLK";
      err "EDESTADDRREQ";
      err "EDOM";
      err "EEXIST";
      err "EFAULT";
      err "EFBIG";
      err "EHOSTDOWN";
      err "EHOSTUNREACH";
      err "EINPROGRESS";
      err "EINTR";
      err "EINVAL";
      err "EIO";
      err "EISCONN";
      err "ELOOP";
      err "EMFILE";
      err "EMLINK";
      err "EMSGSIZE";
      err "ENAMETOOLONG";
      err "ENETDOWN";
      err "ENETRESET";
      err "ENETUNREACH";
      err "ENFILE";
      err "ENOBUFS";
      err "ENOENT";
      err "ENOLCK";
      err "ENOMEM";
      err "ENOPROTOOPT";
      err "ENOSPC";
      err "ENOSYS";
      err "ENOTCONN";
      err "ENOTEMPTY";
      err "ENOTSOCK";
      err "ENOTTY";
      err "EOPNOTSUPP";
      err "EPERM";
      err "EPFNOSUPPORT";
      err "EPIPE";
      err "EPROTONOSUPPORT";
      err "EPROTOTYPE";
      err "ERANGE";
      err "EROFS";
      err "ESHUTDOWN";
      err "ESOCKTNOSUPPORT";
      err "ESPIPE";
      err "ETIMEDOUT";
      err "ETOOMANYREFS";
      err "EWOULDBLOCK";
      err "EXDEV";
    ]

let unix_file_kind_decl =
  let kind name =
    constructor name (TypeScheme.of_type unix_file_kind_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.file_kind"
    ~type_name:"file_kind"
    [
      kind "S_REG";
      kind "S_DIR";
      kind "S_LNK";
      kind "S_BLK";
      kind "S_CHR";
      kind "S_FIFO";
      kind "S_SOCK";
    ]

let unix_stats_decl =
  record_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.stats"
    ~type_name:"stats"
    [
      label "st_dev" TypeRepr.int;
      label "st_ino" TypeRepr.int;
      label "st_kind" unix_file_kind_type;
      label "st_perm" TypeRepr.int;
      label "st_nlink" TypeRepr.int;
      label "st_uid" TypeRepr.int;
      label "st_gid" TypeRepr.int;
      label "st_rdev" TypeRepr.int;
      label "st_size" TypeRepr.int;
      label "st_atime" TypeRepr.float;
      label "st_mtime" TypeRepr.float;
      label "st_ctime" TypeRepr.float;
    ]

let unix_seek_command_decl =
  let command name =
    constructor name (TypeScheme.of_type unix_seek_command_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.seek_command"
    ~type_name:"seek_command"
    [
      command "SEEK_SET";
      command "SEEK_CUR";
      command "SEEK_END";
    ]

let unix_lock_command_decl =
  let command name =
    constructor name (TypeScheme.of_type unix_lock_command_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.lock_command"
    ~type_name:"lock_command"
    [
      command "F_ULOCK";
      command "F_LOCK";
      command "F_TLOCK";
      command "F_TEST";
      command "F_RLOCK";
      command "F_TRLOCK";
    ]

let unix_process_status_decl =
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.process_status"
    ~type_name:"process_status"
    [
      constructor "WEXITED" (TypeScheme.of_type (arrow TypeRepr.int unix_process_status_type));
      constructor "WSIGNALED" (TypeScheme.of_type (arrow TypeRepr.int unix_process_status_type));
      constructor "WSTOPPED" (TypeScheme.of_type (arrow TypeRepr.int unix_process_status_type));
    ]

let unix_wait_flag_decl =
  let flag name =
    constructor name (TypeScheme.of_type unix_wait_flag_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.wait_flag"
    ~type_name:"wait_flag"
    [
      flag "WNOHANG";
      flag "WUNTRACED";
    ]

let unix_socket_domain_decl =
  let domain name =
    constructor name (TypeScheme.of_type unix_socket_domain_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.socket_domain"
    ~type_name:"socket_domain"
    [
      domain "PF_UNIX";
      domain "PF_INET";
      domain "PF_INET6";
    ]

let unix_socket_type_decl =
  let socket_type name =
    constructor name (TypeScheme.of_type unix_socket_type_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.socket_type"
    ~type_name:"socket_type"
    [
      socket_type "SOCK_STREAM";
      socket_type "SOCK_DGRAM";
    ]

let unix_inet_addr_decl = abstract_type_decl
  ~scope_path:IdentPath.empty
  ~path:"Unix.inet_addr"
  ~type_name:"inet_addr"
  ()

let unix_sockaddr_decl =
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.sockaddr"
    ~type_name:"sockaddr"
    [
      constructor "ADDR_UNIX" (TypeScheme.of_type (arrow TypeRepr.string unix_sockaddr_type));
      constructor "ADDR_INET"
        (TypeScheme.of_type (arrow (TypeRepr.tuple [ unix_inet_addr_type; TypeRepr.int ]) unix_sockaddr_type));
    ]

let unix_addr_info_decl =
  record_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.addr_info"
    ~type_name:"addr_info"
    [
      label "ai_family" unix_socket_domain_type;
      label "ai_socktype" unix_socket_type_type;
      label "ai_protocol" TypeRepr.int;
      label "ai_addr" unix_sockaddr_type;
    ]

let unix_setattr_when_decl =
  let when_ name =
    constructor name (TypeScheme.of_type unix_setattr_when_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.setattr_when"
    ~type_name:"setattr_when"
    [
      when_ "TCSANOW";
      when_ "TCSADRAIN";
      when_ "TCSAFLUSH";
    ]

let unix_socket_bool_option_decl =
  let option name =
    constructor name (TypeScheme.of_type unix_socket_bool_option_type)
  in
  variant_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.socket_bool_option"
    ~type_name:"socket_bool_option"
    [
      option "SO_REUSEADDR";
      option "SO_REUSEPORT";
      option "TCP_NODELAY";
    ]

let unix_msg_flag_decl = abstract_type_decl
  ~scope_path:IdentPath.empty
  ~path:"Unix.msg_flag"
  ~type_name:"msg_flag"
  ()

let unix_getaddrinfo_option_decl = abstract_type_decl
  ~scope_path:IdentPath.empty
  ~path:"Unix.getaddrinfo_option"
  ~type_name:"getaddrinfo_option"
  ()

let unix_terminal_io_decl =
  record_type_decl
    ~scope_path:IdentPath.empty
    ~path:"Unix.terminal_io"
    ~type_name:"terminal_io"
    [
      label ~mutable_:true "c_ignbrk" TypeRepr.bool;
      label ~mutable_:true "c_brkint" TypeRepr.bool;
      label ~mutable_:true "c_ignpar" TypeRepr.bool;
      label ~mutable_:true "c_parmrk" TypeRepr.bool;
      label ~mutable_:true "c_inpck" TypeRepr.bool;
      label ~mutable_:true "c_istrip" TypeRepr.bool;
      label ~mutable_:true "c_inlcr" TypeRepr.bool;
      label ~mutable_:true "c_igncr" TypeRepr.bool;
      label ~mutable_:true "c_icrnl" TypeRepr.bool;
      label ~mutable_:true "c_ixon" TypeRepr.bool;
      label ~mutable_:true "c_ixoff" TypeRepr.bool;
      label ~mutable_:true "c_opost" TypeRepr.bool;
      label ~mutable_:true "c_obaud" TypeRepr.int;
      label ~mutable_:true "c_ibaud" TypeRepr.int;
      label ~mutable_:true "c_csize" TypeRepr.int;
      label ~mutable_:true "c_cstopb" TypeRepr.int;
      label ~mutable_:true "c_cread" TypeRepr.bool;
      label ~mutable_:true "c_parenb" TypeRepr.bool;
      label ~mutable_:true "c_parodd" TypeRepr.bool;
      label ~mutable_:true "c_hupcl" TypeRepr.bool;
      label ~mutable_:true "c_clocal" TypeRepr.bool;
      label ~mutable_:true "c_isig" TypeRepr.bool;
      label ~mutable_:true "c_icanon" TypeRepr.bool;
      label ~mutable_:true "c_noflsh" TypeRepr.bool;
      label ~mutable_:true "c_echo" TypeRepr.bool;
      label ~mutable_:true "c_echoe" TypeRepr.bool;
      label ~mutable_:true "c_echok" TypeRepr.bool;
      label ~mutable_:true "c_echonl" TypeRepr.bool;
      label ~mutable_:true "c_vintr" TypeRepr.char;
      label ~mutable_:true "c_vquit" TypeRepr.char;
      label ~mutable_:true "c_verase" TypeRepr.char;
      label ~mutable_:true "c_vkill" TypeRepr.char;
      label ~mutable_:true "c_veof" TypeRepr.char;
      label ~mutable_:true "c_veol" TypeRepr.char;
      label ~mutable_:true "c_vmin" TypeRepr.int;
      label ~mutable_:true "c_vtime" TypeRepr.int;
      label ~mutable_:true "c_vstart" TypeRepr.char;
      label ~mutable_:true "c_vstop" TypeRepr.char;
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
  ("abs", int32_unop);
  ("add", int32_binop);
  ("bits_of_float", stdlib_int32_bits_of_float);
  ("compare", int32_compare);
  ("div", int32_binop);
  ("equal", int32_equal);
  ("float_of_bits", stdlib_int32_float_of_bits);
  ("hash", stdlib_int32_hash);
  ("logand", int32_binop);
  ("lognot", int32_unop);
  ("logor", int32_binop);
  ("logxor", int32_binop);
  ("max", int32_binop);
  ("max_int", int32_value);
  ("min", int32_binop);
  ("min_int", int32_value);
  ("minus_one", int32_value);
  ("mul", int32_binop);
  ("neg", int32_unop);
  ("of_float", stdlib_int32_of_float);
  ("of_int", stdlib_int32_of_int);
  ("of_string", stdlib_int32_of_string);
  ("of_string_opt", stdlib_int32_of_string_opt);
  ("one", int32_value);
  ("pred", int32_unop);
  ("rem", int32_binop);
  ("seeded_hash", stdlib_int32_seeded_hash);
  ("shift_left", TypeScheme.of_type (arrow int32_type (arrow TypeRepr.int int32_type)));
  ("shift_right", TypeScheme.of_type (arrow int32_type (arrow TypeRepr.int int32_type)));
  ("shift_right_logical", TypeScheme.of_type (arrow int32_type (arrow TypeRepr.int int32_type)));
  ("sub", int32_binop);
  ("succ", int32_unop);
  ("to_float", stdlib_int32_to_float);
  ("to_int", stdlib_int32_to_int);
  ("to_string", stdlib_int32_to_string);
  ("unsigned_compare", int32_compare);
  ("unsigned_div", int32_binop);
  ("unsigned_rem", int32_binop);
  ("unsigned_to_int", stdlib_int32_unsigned_to_int);
  ("zero", int32_value);
]

let stdlib_int64_exports = [
  ("abs", int64_unop);
  ("add", int64_binop);
  ("bits_of_float", stdlib_int64_bits_of_float);
  ("compare", int64_compare);
  ("div", int64_binop);
  ("equal", int64_equal);
  ("float_of_bits", stdlib_int64_float_of_bits);
  ("hash", stdlib_int64_hash);
  ("logand", int64_binop);
  ("lognot", int64_unop);
  ("logor", int64_binop);
  ("logxor", int64_binop);
  ("max", int64_binop);
  ("max_int", int64_value);
  ("min", int64_binop);
  ("min_int", int64_value);
  ("minus_one", int64_value);
  ("mul", int64_binop);
  ("neg", int64_unop);
  ("of_float", stdlib_int64_of_float);
  ("of_int", stdlib_int64_of_int);
  ("of_int32", stdlib_int64_of_int32);
  ("of_nativeint", stdlib_int64_of_nativeint);
  ("of_string", stdlib_int64_of_string);
  ("of_string_opt", stdlib_int64_of_string_opt);
  ("one", int64_value);
  ("pred", int64_unop);
  ("rem", int64_binop);
  ("seeded_hash", stdlib_int64_seeded_hash);
  ("shift_left", TypeScheme.of_type (arrow int64_type (arrow TypeRepr.int int64_type)));
  ("shift_right", TypeScheme.of_type (arrow int64_type (arrow TypeRepr.int int64_type)));
  ("shift_right_logical", TypeScheme.of_type (arrow int64_type (arrow TypeRepr.int int64_type)));
  ("sub", int64_binop);
  ("succ", int64_unop);
  ("to_float", stdlib_int64_to_float);
  ("to_int", stdlib_int64_to_int);
  ("to_int32", stdlib_int64_to_int32);
  ("to_nativeint", stdlib_int64_to_nativeint);
  ("to_string", stdlib_int64_to_string);
  ("unsigned_compare", int64_compare);
  ("unsigned_div", int64_binop);
  ("unsigned_rem", int64_binop);
  ("unsigned_to_int", stdlib_int64_unsigned_to_int);
  ("zero", int64_value);
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

let stdlib_random_exports = [
  ("bits", stdlib_random_bits);
  ("bits32", stdlib_random_bits32);
  ("bits64", stdlib_random_bits64);
  ("bool", stdlib_random_bool);
  ("float", stdlib_random_float);
  ("full_init", stdlib_random_full_init);
  ("full_int", stdlib_random_full_int);
  ("get_state", stdlib_random_get_state);
  ("init", stdlib_random_init);
  ("int", stdlib_random_int);
  ("int32", stdlib_random_int32);
  ("int32_in_range", stdlib_random_int32_in_range);
  ("int64", stdlib_random_int64);
  ("int64_in_range", stdlib_random_int64_in_range);
  ("int_in_range", stdlib_random_int_in_range);
  ("self_init", stdlib_random_self_init);
  ("set_state", stdlib_random_set_state);
  ("split", stdlib_random_split);
]

let stdlib_random_state_exports = [
  ("bits", stdlib_random_state_bits);
  ("bits32", stdlib_random_state_bits32);
  ("bits64", stdlib_random_state_bits64);
  ("bool", stdlib_random_state_bool);
  ("copy", stdlib_random_state_copy);
  ("float", stdlib_random_state_float);
  ("full_int", stdlib_random_state_full_int);
  ("int", stdlib_random_state_int);
  ("int32", stdlib_random_state_int32);
  ("int32_in_range", stdlib_random_state_int32_in_range);
  ("int64", stdlib_random_state_int64);
  ("int64_in_range", stdlib_random_state_int64_in_range);
  ("int_in_range", stdlib_random_state_int_in_range);
  ("make", stdlib_random_state_make);
  ("make_self_init", stdlib_random_state_make_self_init);
  ("of_binary_string", stdlib_random_state_of_binary_string);
  ("split", stdlib_random_state_split);
  ("to_binary_string", stdlib_random_state_to_binary_string);
]

let stdlib_type_id_exports = [
  ("make", type_id_make);
  ("uid", type_id_uid);
]

let stdlib_uchar_exports = [
  ("bom", stdlib_uchar_value);
  ("compare", stdlib_uchar_compare);
  ("equal", stdlib_uchar_equal);
  ("hash", stdlib_uchar_hash);
  ("is_char", stdlib_uchar_is_char);
  ("is_valid", stdlib_uchar_is_valid);
  ("max", stdlib_uchar_value);
  ("min", stdlib_uchar_value);
  ("of_char", stdlib_uchar_of_char);
  ("of_int", stdlib_uchar_of_int);
  ("pred", stdlib_uchar_pred_succ);
  ("rep", stdlib_uchar_value);
  ("seeded_hash", stdlib_uchar_seeded_hash);
  ("succ", stdlib_uchar_pred_succ);
  ("to_char", stdlib_uchar_to_char);
  ("to_int", stdlib_uchar_to_int);
  ("unsafe_of_int", stdlib_uchar_of_int);
  ("unsafe_to_char", stdlib_uchar_to_char);
  ("utf_16_byte_length", stdlib_uchar_utf_16_byte_length);
  ("utf_8_byte_length", stdlib_uchar_utf_8_byte_length);
  ("utf_decode", stdlib_uchar_utf_decode);
  ("utf_decode_invalid", stdlib_uchar_utf_decode_invalid);
  ("utf_decode_is_valid", stdlib_uchar_utf_decode_is_valid);
  ("utf_decode_length", stdlib_uchar_utf_decode_length);
  ("utf_decode_uchar", stdlib_uchar_utf_decode_uchar);
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
  ("Unix_error", TypeScheme.of_type
    (arrow (TypeRepr.tuple [ unix_error_type; TypeRepr.string; TypeRepr.string ]) exn_type));
  ("accept", unix_accept);
  ("chdir", TypeScheme.of_type (arrow TypeRepr.string TypeRepr.unit_));
  ("bind", unix_bind);
  ("chmod", unix_chmod);
  ("clear_nonblock", TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.unit_));
  ("close", TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.unit_));
  ("connect", unix_connect);
  ("create_process_env", unix_create_process_env);
  ("dup", unix_dup);
  ("environment", TypeScheme.of_type (arrow TypeRepr.unit_ (TypeRepr.array TypeRepr.string)));
  ("error_message", unix_error_message);
  ("execv", TypeScheme.of_type
    (arrow TypeRepr.string (arrow (TypeRepr.array TypeRepr.string) TypeRepr.unit_)));
  ("fchmod", unix_fchmod);
  ("fstat", unix_fstat);
  ("fsync", unix_fsync);
  ("ftruncate", unix_ftruncate);
  ("getaddrinfo", unix_getaddrinfo);
  ("getcwd", TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.string));
  ("getpid", TypeScheme.of_type (arrow TypeRepr.unit_ TypeRepr.int));
  ("getsockname", unix_getsockname);
  ("inet_addr_of_string", unix_inet_addr_of_string);
  ("kill", TypeScheme.of_type (arrow TypeRepr.int (arrow TypeRepr.int TypeRepr.unit_)));
  ("isatty", TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.bool));
  ("link", unix_link);
  ("listen", unix_listen);
  ("lockf", unix_lockf);
  ("lseek", unix_lseek);
  ("lstat", unix_lstat);
  ("mkdir", unix_mkdir);
  ("openfile", TypeScheme.of_type
    (arrow TypeRepr.string (arrow (TypeRepr.list unix_open_flag_type) (arrow TypeRepr.int unix_file_descr_type))));
  ("pipe", TypeScheme.of_type (arrow TypeRepr.unit_ (TypeRepr.tuple [ unix_file_descr_type; unix_file_descr_type ])));
  ("putenv", TypeScheme.of_type (arrow TypeRepr.string (arrow TypeRepr.string TypeRepr.unit_)));
  ("read", unix_read);
  ("readlink", unix_readlink);
  ("realpath", unix_realpath);
  ("recv", unix_recv);
  ("recvfrom", unix_recvfrom);
  ("rename", unix_rename);
  ("rmdir", unix_rmdir);
  ("send", unix_send);
  ("sendto", unix_sendto);
  ("sleepf", TypeScheme.of_type (arrow TypeRepr.float TypeRepr.unit_));
  ("set_close_on_exec", TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.unit_));
  ("set_nonblock", TypeScheme.of_type (arrow unix_file_descr_type TypeRepr.unit_));
  ("setsockopt", unix_setsockopt);
  ("socket", unix_socket);
  ("stat", unix_stat);
  ("stderr", TypeScheme.of_type unix_file_descr_type);
  ("stdin", TypeScheme.of_type unix_file_descr_type);
  ("stdout", TypeScheme.of_type unix_file_descr_type);
  ("string_of_inet_addr", unix_string_of_inet_addr);
  ("symlink", unix_symlink);
  ("tcgetattr", unix_tcgetattr);
  ("tcsetattr", unix_tcsetattr);
  ("waitpid", unix_waitpid);
]

let unixlabels_exports = [
  ("read", unixlabels_read);
  ("write", unixlabels_write);
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
  ("exit", polymorphic_exit);
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
    @ prefix_exports "Random" stdlib_random_exports
    @ prefix_exports "Random.State" stdlib_random_state_exports
    @ prefix_exports "Seq" stdlib_seq_exports
    @ prefix_exports "Obj" obj_exports
    @ prefix_exports "Printexc" stdlib_printexc_exports
    @ prefix_exports "String" stdlib_string_exports
    @ prefix_exports "Sys" stdlib_sys_exports
    @ prefix_exports "Type.Id" stdlib_type_id_exports
    @ prefix_exports "Uchar" stdlib_uchar_exports,
    [
      domain_dls_key_type_decl;
      domain_type_decl;
      stdlib_effect_t_decl;
      stdlib_buffer_t_decl;
      stdlib_bytes_t_decl;
      stdlib_fpclass_decl;
      stdlib_hashtbl_t_decl;
      stdlib_int32_t_decl;
      stdlib_int64_t_decl;
      stdlib_random_state_t_decl;
      stdlib_seq_node_decl;
      stdlib_seq_t_decl;
      stdlib_sys_signal_behavior_decl;
      stdlib_type_eq_decl;
      stdlib_type_id_decl;
      stdlib_uchar_t_decl;
      stdlib_uchar_utf_decode_decl;
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
  ("Dynlink", [], []);
  ("Float", stdlib_float_exports, []);
  ("Hashtbl", hashtbl_exports, [ hashtbl_t_decl ]);
  ("Int", stdlib_int_exports, []);
  ("List", stdlib_list_exports, []);
  ("Obj", obj_exports, []);
  ("Printexc", stdlib_printexc_exports, []);
  ("String", stdlib_string_exports, []);
  (
    "Unix",
    unix_exports,
    [
      unix_addr_info_decl;
      unix_error_decl;
      unix_file_descr_decl;
      unix_file_kind_decl;
      unix_getaddrinfo_option_decl;
      unix_inet_addr_decl;
      unix_lock_command_decl;
      unix_msg_flag_decl;
      unix_open_flag_decl;
      unix_process_status_decl;
      unix_seek_command_decl;
      unix_setattr_when_decl;
      unix_socket_bool_option_decl;
      unix_socket_domain_decl;
      unix_socket_type_decl;
      unix_sockaddr_decl;
      unix_stats_decl;
      unix_terminal_io_decl;
      unix_wait_flag_decl;
    ]
  );
  ("UnixLabels", unixlabels_exports, []);
]
|> List.mapi
  (fun index (module_name, exports, type_decls) ->
    module_typings
      ~source_id:(SourceId.of_int (ocaml_stdlib_module_source_id_base - index))
      ~module_name
      ~type_decls
      exports)
