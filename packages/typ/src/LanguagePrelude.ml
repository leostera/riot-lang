open Std
open Model

type env = (IdentPath.t * TypeScheme.t) list

let monomorphic = fun ty -> TypeScheme.of_type ty

let var = fun id -> TypeRepr.make_var id

let arrow = fun ?(label = TypeRepr.Nolabel) lhs rhs -> TypeRepr.arrow ~label ~lhs ~rhs

let qualified = fun module_name name ->
  IdentPath.append_name (IdentPath.of_name module_name) name

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

let prelude_list_type_constructor_id = BuiltinTypeConstructors.list_type_constructor_id

let prelude_nil_constructor_id = ConstructorId.of_int (-1)

let prelude_cons_constructor_id = ConstructorId.of_int (-2)

let bindings = [
  (IdentPath.of_name "[]", list_nil);
  (IdentPath.of_name "::", list_cons);
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
        nonrec_ = false;
        param_ids = [ 0 ];
        param_variances = [ TypeDecl.Covariant ];
        constructors = [
          {
            TypeDecl.constructor_id = prelude_nil_constructor_id;
            name = "[]";
            scheme = list_nil;
            inline_record_labels = None
          };
          {
            TypeDecl.constructor_id = prelude_cons_constructor_id;
            name = "::";
            scheme = list_cons;
            inline_record_labels = None
          };
        ];
        labels = [];
        manifest = None;
      };
  } ]
