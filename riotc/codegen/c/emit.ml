// C emission for the first native RiotML target.
use Lambda.Ir.{LambdaInteger, LambdaBoolean, LambdaAdd, LambdaSubtract, LambdaMultiply, LambdaDivide, LambdaNegate, Entry}

let emit_expr = fn expression ->
  match expression {
    | LambdaInteger(value) -> "rt_int(" + value + ")"
    | LambdaBoolean(true) -> "rt_bool(1)"
    | LambdaBoolean(false) -> "rt_bool(0)"
    | LambdaAdd(left, right) -> "rt_i32_add(" + emit_expr left + "," + emit_expr right + ")"
    | LambdaSubtract(left, right) -> "rt_i32_sub(" + emit_expr left + "," + emit_expr right + ")"
    | LambdaMultiply(left, right) -> "rt_i32_mul(" + emit_expr left + "," + emit_expr right + ")"
    | LambdaDivide(left, right) -> "rt_i32_div(" + emit_expr left + "," + emit_expr right + ")"
    | LambdaNegate(operand) -> "rt_i32_neg(" + emit_expr operand + ")"
  }

let emit_program = fn module ->
  match module {
    | Entry(expression) ->
      "#include \"runtime.h\"\n"
      + "RtValue rt_riotml_entry(RtValue environment, RtValue argument) {\n"
      + "  (void)environment; (void)argument;\n  return " + emit_expr expression + ";\n}\n"
      + "int main(int argc, char **argv) {\n"
      + "  rt_set_args(argc, argv);\n"
      + "  RtValue entry = rt_function(rt_riotml_entry, rt_unit());\n"
      + "  return (int)rt_as_int(rt_apply(entry, rt_unit()));\n}\n"
  }
