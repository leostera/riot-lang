open Std

type env = (string * TypeScheme.t) list

let monomorphic = fun ty -> TypeScheme.Forall ([], ty)

let var = fun id -> TypeRepr.make_var id

let arrow = fun ?(label = TypeRepr.Nolabel) lhs rhs -> TypeRepr.Arrow { label; lhs; rhs }

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

let failure_constructor = monomorphic
  (arrow TypeRepr.String (TypeRepr.Named { name = "exn"; arguments = [] }))

let raise_fn = TypeScheme.Forall (
  [ 0 ],
  arrow (TypeRepr.Named { name = "exn"; arguments = [] }) (var 0)
)

let bindings = [
  ("[]", list_nil);
  ("::", list_cons);
  ("None", option_none);
  ("Some", option_some);
  ("Ok", result_ok);
  ("Error", result_error);
  ("+", int_binop);
  ("-", int_binop);
  ("*", int_binop);
  ("/", int_binop);
  ("+.", float_binop);
  ("-.", float_binop);
  ("*.", float_binop);
  ("/.", float_binop);
  ("=", polymorphic_eq);
  ("!=", polymorphic_eq);
  ("<", polymorphic_compare);
  ("<=", polymorphic_compare);
  (">", polymorphic_compare);
  (">=", polymorphic_compare);
  ("&&", bool_binop);
  ("||", bool_binop);
  ("not", monomorphic (arrow TypeRepr.Bool TypeRepr.Bool));
  ("|>", polymorphic_pipe);
  ("^", monomorphic (arrow TypeRepr.String (arrow TypeRepr.String TypeRepr.String)));
  ("Failure", failure_constructor);
  ("raise", raise_fn);
]
