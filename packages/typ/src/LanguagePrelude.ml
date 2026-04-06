open Std
open Model

type env = (IdentPath.t * TypeScheme.t) list

let monomorphic = fun ty -> TypeScheme.Forall ([], ty)

let var = fun id -> TypeRepr.make_var id

let arrow = fun ?(label = TypeRepr.Nolabel) lhs rhs -> TypeRepr.Arrow { label; lhs; rhs }

let bare_named = fun name -> TypeRepr.Named { name = IdentPath.of_name name; arguments = [] }

let qualified = fun module_name name ->
  IdentPath.append_name (IdentPath.of_name module_name) name

let named = fun path -> TypeRepr.Named { name = path; arguments = [] }

let polymorphic_eq =
  let lhs = var 0 in
  TypeScheme.Forall ([ 0 ], arrow lhs (arrow lhs TypeRepr.Bool))

let polymorphic_compare =
  let lhs = var 0 in
  TypeScheme.Forall ([ 0 ], arrow lhs (arrow lhs TypeRepr.Bool))

let polymorphic_pipe =
  let input = var 0 in
  let output = var 1 in
  TypeScheme.Forall ([ 0; 1 ], arrow input (arrow (arrow input output) output))

let int_binop = monomorphic (arrow TypeRepr.Int (arrow TypeRepr.Int TypeRepr.Int))

let float_binop = monomorphic (arrow TypeRepr.Float (arrow TypeRepr.Float TypeRepr.Float))

let bool_binop = monomorphic (arrow TypeRepr.Bool (arrow TypeRepr.Bool TypeRepr.Bool))

let list_nil =
  let element = var 0 in
  TypeScheme.Forall ([ 0 ], TypeRepr.List element)

let list_cons =
  let element = var 0 in
  TypeScheme.Forall ([ 0 ], arrow element (arrow (TypeRepr.List element) (TypeRepr.List element)))

let option_none =
  let element = var 0 in
  TypeScheme.Forall ([ 0 ], TypeRepr.Option element)

let option_some =
  let element = var 0 in
  TypeScheme.Forall ([ 0 ], arrow element (TypeRepr.Option element))

let result_ok =
  let ok_ty = var 0 in
  let err_ty = var 1 in
  TypeScheme.Forall ([ 1; 0 ], arrow ok_ty (TypeRepr.Result (ok_ty, err_ty)))

let result_error =
  let ok_ty = var 0 in
  let err_ty = var 1 in
  TypeScheme.Forall ([ 1; 0 ], arrow err_ty (TypeRepr.Result (ok_ty, err_ty)))

let failure_constructor = monomorphic (arrow TypeRepr.String (bare_named "exn"))

let raise_fn = TypeScheme.Forall (
  [ 0 ],
  arrow (bare_named "exn") (var 0)
)

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
  (IdentPath.of_name "not", monomorphic (arrow TypeRepr.Bool TypeRepr.Bool));
  (IdentPath.of_name "|>", polymorphic_pipe);
  (IdentPath.of_name "^", monomorphic (arrow TypeRepr.String (arrow TypeRepr.String TypeRepr.String)));
  (IdentPath.of_name "Failure", failure_constructor);
  (IdentPath.of_name "raise", raise_fn);
]
