(** JS-native expression constructors shared by lowering subsystems.

    Algorithm:
    - provide small helpers for common JS-native expression shapes such as
      globals, member/index access, calls, operators, arrays, and a few owned
      ambient namespaces like `console`, `process`, `Math`, and `String`

    Effect:
    - keeps JS-native syntax construction consistent across builtin lowering,
      primitive lowering, and future representation-policy work
    - removes duplicated backend-local helpers from the main lowering pass

    Rationale:
    ReScript is the reference for preferring ordinary JS syntax over helper
    calls. This module gives `raml-js` one backend-owned place to express that
    choice without scattering object/member/global construction logic across
    multiple lowering modules. *)
val global: string -> Types.Expr.t

val member: Types.Expr.t -> string -> Types.Expr.t

val index: Types.Expr.t -> Types.Expr.t -> Types.Expr.t

val call: Types.Expr.t -> Types.Expr.t list -> Types.Expr.t

val unary: Types.Operator.unary -> Types.Expr.t -> Types.Expr.t

val binary: Types.Operator.binary -> Types.Expr.t -> Types.Expr.t -> Types.Expr.t

val array: Types.Expr.t list -> Types.Expr.t

val string_constructor: Types.Expr.t -> Types.Expr.t

val console_log: Types.Expr.t list -> Types.Expr.t

val console_error: Types.Expr.t list -> Types.Expr.t

val stdout_write: Types.Expr.t -> Types.Expr.t

val stderr_write: Types.Expr.t -> Types.Expr.t

val math_sqrt: Types.Expr.t -> Types.Expr.t
