open Std
open Model

type env = (SurfacePath.t * TypeScheme.t) list

let monomorphic = TypeScheme.of_type

let var = TypeRepr.make_var

let arrow = fun ?(label = TypeRepr.Nolabel) lhs rhs -> TypeRepr.arrow ~label ~lhs ~rhs

let qualified = fun module_name name -> SurfacePath.append_name (SurfacePath.of_name module_name) name

let polymorphic_eq =
  let lhs = var 0 in TypeScheme.of_explicit ~quantified:[ 0 ] (arrow lhs (arrow lhs TypeRepr.bool))

let polymorphic_compare =
  let lhs = var 0 in TypeScheme.of_explicit ~quantified:[ 0 ] (arrow lhs (arrow lhs TypeRepr.bool))

let polymorphic_pipe =
  let input = var 0 in
  let output = var 1 in TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow input (arrow (arrow input output) output))

let polymorphic_ignore =
  let value = var 0 in TypeScheme.of_explicit ~quantified:[ 0 ] (arrow value TypeRepr.unit_)

let int_binop = monomorphic (arrow TypeRepr.int (arrow TypeRepr.int TypeRepr.int))

let float_binop = monomorphic (arrow TypeRepr.float (arrow TypeRepr.float TypeRepr.float))

let bool_binop = monomorphic (arrow TypeRepr.bool (arrow TypeRepr.bool TypeRepr.bool))

let bool_unop = monomorphic (arrow TypeRepr.bool TypeRepr.bool)

let list_nil =
  let element = var 0 in TypeScheme.of_explicit ~quantified:[ 0 ] (TypeRepr.list element)

let list_cons =
  let element = var 0 in TypeScheme.of_explicit ~quantified:[ 0 ] (arrow element (arrow (TypeRepr.list element) (TypeRepr.list element)))

let option_none =
  let element = var 0 in TypeScheme.of_explicit ~quantified:[ 0 ] (TypeRepr.option element)

let option_some =
  let element = var 0 in TypeScheme.of_explicit ~quantified:[ 0 ] (arrow element (TypeRepr.option element))

let result_ok_constructor =
  let ok_ty = var 0 in
  let error_ty = var 1 in TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow ok_ty (TypeRepr.result ok_ty error_ty))

let result_error_constructor =
  let ok_ty = var 0 in
  let error_ty = var 1 in TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow error_ty (TypeRepr.result ok_ty error_ty))

let prelude_list_type_constructor_id = BuiltinTypeConstructors.list_type_constructor_id

let prelude_nil_constructor_id = ConstructorId.of_int (-1)

let prelude_cons_constructor_id = ConstructorId.of_int (-2)

let prelude_option_type_constructor_id = TypeConstructorId.of_path (SurfacePath.of_name "option")

let prelude_result_type_constructor_id = TypeConstructorId.of_path (SurfacePath.of_name "result")

let prelude_none_constructor_id = ConstructorId.of_int (-3)

let prelude_some_constructor_id = ConstructorId.of_int (-4)

let prelude_ok_constructor_id = ConstructorId.of_int (-5)

let prelude_error_constructor_id = ConstructorId.of_int (-6)

let bindings =
  [
    SurfacePath.of_name "[]", list_nil;
    SurfacePath.of_name "::", list_cons;
    SurfacePath.of_name "+", int_binop;
    SurfacePath.of_name "-", int_binop;
    SurfacePath.of_name "*", int_binop;
    SurfacePath.of_name "/", int_binop;
    SurfacePath.of_name "+.", float_binop;
    SurfacePath.of_name "-.", float_binop;
    SurfacePath.of_name "*.", float_binop;
    SurfacePath.of_name "/.", float_binop;
    SurfacePath.of_name "=", polymorphic_eq;
    SurfacePath.of_name "!=", polymorphic_eq;
    SurfacePath.of_name "<>", polymorphic_eq;
    SurfacePath.of_name "<", polymorphic_compare;
    SurfacePath.of_name "<=", polymorphic_compare;
    SurfacePath.of_name ">", polymorphic_compare;
    SurfacePath.of_name ">=", polymorphic_compare;
    SurfacePath.of_name "not", bool_unop;
    SurfacePath.of_name "&&", bool_binop;
    SurfacePath.of_name "||", bool_binop;
    SurfacePath.of_name "ignore", polymorphic_ignore;
    SurfacePath.of_name "|>", polymorphic_pipe;
    SurfacePath.of_name "^", monomorphic (arrow TypeRepr.string (arrow TypeRepr.string TypeRepr.string));
  ]

let type_decls =
  [
    {
      FileSummary.scope_path = SurfacePath.empty;
      declaration = {
        TypeDecl.type_constructor_id = prelude_list_type_constructor_id;
        type_name = "list";
        nonrec_ = false;
        param_ids = [ 0 ];
        param_variances = [ TypeDecl.Covariant ];
        constructors = [
          {
            TypeDecl.constructor_id = prelude_nil_constructor_id;
            name = "[]";
            scheme = list_nil;
            generalized = false;
            inline_record_labels = None
          };
          {
            TypeDecl.constructor_id = prelude_cons_constructor_id;
            name = "::";
            scheme = list_cons;
            generalized = false;
            inline_record_labels = None
          };
        ];
        labels = [];
        manifest = None
      }
    };
    {
      FileSummary.scope_path = SurfacePath.empty;
      declaration = {
        TypeDecl.type_constructor_id = prelude_option_type_constructor_id;
        type_name = "option";
        nonrec_ = false;
        param_ids = [ 0 ];
        param_variances = [ TypeDecl.Covariant ];
        constructors = [
          {
            TypeDecl.constructor_id = prelude_none_constructor_id;
            name = "None";
            scheme = option_none;
            generalized = false;
            inline_record_labels = None
          };
          {
            TypeDecl.constructor_id = prelude_some_constructor_id;
            name = "Some";
            scheme = option_some;
            generalized = false;
            inline_record_labels = None
          };
        ];
        labels = [];
        manifest = None
      }
    };
    {
      FileSummary.scope_path = SurfacePath.empty;
      declaration = {
        TypeDecl.type_constructor_id = prelude_result_type_constructor_id;
        type_name = "result";
        nonrec_ = false;
        param_ids = [ 0; 1 ];
        param_variances = [ TypeDecl.Covariant; TypeDecl.Covariant ];
        constructors = [
          {
            TypeDecl.constructor_id = prelude_ok_constructor_id;
            name = "Ok";
            scheme = result_ok_constructor;
            generalized = false;
            inline_record_labels = None
          };
          {
            TypeDecl.constructor_id = prelude_error_constructor_id;
            name = "Error";
            scheme = result_error_constructor;
            generalized = false;
            inline_record_labels = None
          };
        ];
        labels = [];
        manifest = None
      }
    };
  ]
