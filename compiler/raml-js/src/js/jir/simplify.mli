val block: Types.Statement.t list -> Types.Statement.t list

val function_body: Types.Statement.t list -> Types.Statement.t list

val conditional:
  condition:Types.Expr.t ->
  then_:Types.Statement.t list ->
  else_:Types.Statement.t list ->
  Types.Statement.t list

val effect_expression: Types.Expr.t -> Types.Statement.t list
