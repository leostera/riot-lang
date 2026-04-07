open Std
open Model

type env = (IdentPath.t * TypeScheme.t) list

let monomorphic = fun ty -> TypeScheme.of_type ty

let var = fun id -> TypeRepr.make_var id

let arrow = fun ?(label = TypeRepr.Nolabel) lhs rhs -> TypeRepr.arrow ~label ~lhs ~rhs

let bare_named = fun name ->
  TypeRepr.named ~type_constructor_id:None ~name:(IdentPath.of_name name) ~arguments:[]

let qualified = fun module_name name ->
  IdentPath.append_name (IdentPath.of_name module_name) name

let named = fun path -> TypeRepr.named ~type_constructor_id:None ~name:path ~arguments:[]

let polymorphic_eq =
  let lhs = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow lhs (arrow lhs TypeRepr.bool))

let polymorphic_compare =
  let lhs = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow lhs (arrow lhs TypeRepr.bool))

let polymorphic_pipe =
  let input = var 0 in
  let output = var 1 in
  TypeScheme.of_explicit ~quantified:[ 0; 1 ] (arrow input (arrow (arrow input output) output))

let int_binop = monomorphic (arrow TypeRepr.int (arrow TypeRepr.int TypeRepr.int))

let float_binop = monomorphic (arrow TypeRepr.float (arrow TypeRepr.float TypeRepr.float))

let bool_binop = monomorphic (arrow TypeRepr.bool (arrow TypeRepr.bool TypeRepr.bool))

let list_nil =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (TypeRepr.list element)

let list_cons =
  let element = var 0 in
  TypeScheme.of_explicit
    ~quantified:[ 0 ]
    (arrow element (arrow (TypeRepr.list element) (TypeRepr.list element)))

let option_none =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (TypeRepr.option element)

let option_some =
  let element = var 0 in
  TypeScheme.of_explicit ~quantified:[ 0 ] (arrow element (TypeRepr.option element))

let result_ok =
  let ok_ty = var 0 in
  let err_ty = var 1 in
  TypeScheme.of_explicit ~quantified:[ 1; 0 ] (arrow ok_ty (TypeRepr.result ok_ty err_ty))

let result_error =
  let ok_ty = var 0 in
  let err_ty = var 1 in
  TypeScheme.of_explicit ~quantified:[ 1; 0 ] (arrow err_ty (TypeRepr.result ok_ty err_ty))

let prelude_list_type_constructor_id = TypeConstructorId.of_int (-1)

let prelude_option_type_constructor_id = TypeConstructorId.of_int (-2)

let prelude_result_type_constructor_id = TypeConstructorId.of_int (-3)

let prelude_nil_constructor_id = ConstructorId.of_int (-1)

let prelude_cons_constructor_id = ConstructorId.of_int (-2)

let prelude_none_constructor_id = ConstructorId.of_int (-3)

let prelude_some_constructor_id = ConstructorId.of_int (-4)

let prelude_ok_constructor_id = ConstructorId.of_int (-5)

let prelude_error_constructor_id = ConstructorId.of_int (-6)

let bindings = [
  (IdentPath.of_name "[]", list_nil);
  (IdentPath.of_name "::", list_cons);
  (IdentPath.of_name "None", option_none);
  (IdentPath.of_name "Some", option_some);
  (IdentPath.of_name "Ok", result_ok);
  (IdentPath.of_name "Error", result_error);
  (IdentPath.of_name "+", int_binop);
  (IdentPath.of_name "-", int_binop);
  (IdentPath.of_name "*", int_binop);
  (IdentPath.of_name "/", int_binop);
  (IdentPath.of_name "+.", float_binop);
  (IdentPath.of_name "-.", float_binop);
  (IdentPath.of_name "*.", float_binop);
  (IdentPath.of_name "/.", float_binop);
  (IdentPath.of_name "=", polymorphic_eq);
  (IdentPath.of_name "!=", polymorphic_eq);
  (IdentPath.of_name "<", polymorphic_compare);
  (IdentPath.of_name "<=", polymorphic_compare);
  (IdentPath.of_name ">", polymorphic_compare);
  (IdentPath.of_name ">=", polymorphic_compare);
  (IdentPath.of_name "&&", bool_binop);
  (IdentPath.of_name "||", bool_binop);
  (IdentPath.of_name "not", monomorphic (arrow TypeRepr.bool TypeRepr.bool));
  (IdentPath.of_name "|>", polymorphic_pipe);
  (
    IdentPath.of_name "^",
    monomorphic (arrow TypeRepr.string (arrow TypeRepr.string TypeRepr.string))
  );
]

let type_decls = [ {
    FileSummary.scope_path = IdentPath.empty;
    declaration =
      {
        TypeDecl.type_constructor_id = prelude_list_type_constructor_id;
        type_name = "list";
        param_ids = [ 0 ];
        param_variances = [ TypeDecl.Covariant ];
        constructors = [
          { TypeDecl.constructor_id = prelude_nil_constructor_id; name = "[]"; scheme = list_nil };
          { TypeDecl.constructor_id = prelude_cons_constructor_id; name = "::"; scheme = list_cons };
        ];
        labels = [];
        manifest = None;
      };
  }; {
    FileSummary.scope_path = IdentPath.empty;
    declaration =
      {
        TypeDecl.type_constructor_id = prelude_option_type_constructor_id;
        type_name = "option";
        param_ids = [ 0 ];
        param_variances = [ TypeDecl.Covariant ];
        constructors = [
          {
            TypeDecl.constructor_id = prelude_none_constructor_id;
            name = "None";
            scheme = option_none
          };
          {
            TypeDecl.constructor_id = prelude_some_constructor_id;
            name = "Some";
            scheme = option_some
          };
        ];
        labels = [];
        manifest = None;
      };
  }; {
    FileSummary.scope_path = IdentPath.empty;
    declaration =
      {
        TypeDecl.type_constructor_id = prelude_result_type_constructor_id;
        type_name = "result";
        param_ids = [ 0; 1 ];
        param_variances = [ TypeDecl.Covariant; TypeDecl.Covariant ];
        constructors = [
          { TypeDecl.constructor_id = prelude_ok_constructor_id; name = "Ok"; scheme = result_ok };
          {
            TypeDecl.constructor_id = prelude_error_constructor_id;
            name = "Error";
            scheme = result_error
          };
        ];
        labels = [];
        manifest = None;
      };
  }; ]
