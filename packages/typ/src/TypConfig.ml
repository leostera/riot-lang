open Std

type env = (string * TypeScheme.t) list

type t = {
  prelude: env;
  ambient: env;
}

let monomorphic = fun ty -> TypeScheme.Forall ([], ty)

let unknown = TypeRepr.Hole (-1)

let polymorphic_eq =
  let lhs = TypeRepr.Var { id = 0; link = None } in
  TypeScheme.Forall ([ 0 ], TypeRepr.Arrow (lhs, TypeRepr.Arrow (lhs, TypeRepr.Bool)))

let polymorphic_compare =
  let lhs = TypeRepr.Var { id = 0; link = None } in
  TypeScheme.Forall ([ 0 ], TypeRepr.Arrow (lhs, TypeRepr.Arrow (lhs, TypeRepr.Bool)))

let polymorphic_pipe =
  let input = TypeRepr.Var { id = 0; link = None } in
  let output = TypeRepr.Var { id = 1; link = None } in
  TypeScheme.Forall (
    [ 0; 1 ],
    TypeRepr.Arrow (input, TypeRepr.Arrow (TypeRepr.Arrow (input, output), output))
  )

let int_binop =
  monomorphic (TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Int)))

let float_binop =
  monomorphic (TypeRepr.Arrow (TypeRepr.Float, TypeRepr.Arrow (TypeRepr.Float, TypeRepr.Float)))

let option_none =
  TypeScheme.Forall ([ 0 ], TypeRepr.Option (TypeRepr.Var { id = 0; link = None }))

let option_some =
  TypeScheme.Forall ([
    0
  ], TypeRepr.Arrow (TypeRepr.Var { id = 0; link = None }, TypeRepr.Option (TypeRepr.Var {
    id = 0;
    link = None
  })))

let result_ok =
  TypeScheme.Forall ([
    1;
    0
  ], TypeRepr.Arrow (
    TypeRepr.Var { id = 0; link = None },
    TypeRepr.Result (TypeRepr.Var { id = 0; link = None }, TypeRepr.Var { id = 1; link = None })
  ))

let result_error =
  TypeScheme.Forall ([
    1;
    0
  ], TypeRepr.Arrow (
    TypeRepr.Var { id = 1; link = None },
    TypeRepr.Result (TypeRepr.Var { id = 0; link = None }, TypeRepr.Var { id = 1; link = None })
  ))

let runtime_args = TypeScheme.Forall ([], TypeRepr.Hole (-2))

let runtime_run =
  TypeScheme.Forall ([
    2;
    1;
    0
  ], TypeRepr.Arrow (
    TypeRepr.Var { id = 0; link = None },
    TypeRepr.Arrow (TypeRepr.Var { id = 1; link = None }, TypeRepr.Arrow (TypeRepr.Unit, TypeRepr.Var {
      id = 2;
      link = None
    }))
  ))

let default = {
  prelude = [
    ("None", option_none);
    ("Some", option_some);
    ("Ok", result_ok);
    ("Error", result_error);
    ("+", int_binop);
    ("-", int_binop);
    ("*", int_binop);
    ("/", int_binop);
    ("^", monomorphic (TypeRepr.Arrow (TypeRepr.String, TypeRepr.Arrow (TypeRepr.String, TypeRepr.String))));
    ("=", polymorphic_eq);
    ("!=", polymorphic_eq);
    ("<", polymorphic_compare);
    ("<=", polymorphic_compare);
    (">", polymorphic_compare);
    (">=", polymorphic_compare);
    ("+.", float_binop);
    ("-.", float_binop);
    ("*.", float_binop);
    ("/.", float_binop);
    ("|>", polymorphic_pipe);
    ("not", monomorphic (TypeRepr.Arrow (TypeRepr.Bool, TypeRepr.Bool)));
    ("Int.to_string", monomorphic (TypeRepr.Arrow (TypeRepr.Int, TypeRepr.String)));
    ("Int.min", int_binop);
    ("Int.max", int_binop);
    ("Float.to_string", monomorphic (TypeRepr.Arrow (TypeRepr.Float, TypeRepr.String)));
    ("Float.of_int", monomorphic (TypeRepr.Arrow (TypeRepr.Int, TypeRepr.Float)));
    ("Float.to_int", monomorphic (TypeRepr.Arrow (TypeRepr.Float, TypeRepr.Int)));
    ("Float.pow", monomorphic (TypeRepr.Arrow (TypeRepr.Float, TypeRepr.Arrow (TypeRepr.Float, TypeRepr.Float))));
    ("Float.cbrt", monomorphic (TypeRepr.Arrow (TypeRepr.Float, TypeRepr.Float)));
    ("Float.min", float_binop);
    ("Float.max", float_binop);
    ("Array.length", monomorphic (TypeRepr.Arrow (TypeRepr.Array unknown, TypeRepr.Int)));
    ("Std.println", monomorphic (TypeRepr.Arrow (TypeRepr.String, TypeRepr.Unit)));
    ("Std.Env.args", runtime_args);
    ("Actors.run", runtime_run);
  ]
  ;
  ambient = [];
}

let with_ambient = fun config ~ambient -> { config with ambient }
