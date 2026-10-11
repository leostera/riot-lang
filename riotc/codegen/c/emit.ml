// Emit C for the native first-order Lambda IR.
use Lambda.Ir.{
  LambdaInteger, LambdaBoolean, LambdaBinary, LambdaConditional,
  LambdaNegation, LambdaBinaryExpression, LambdaConditionalExpression,
  AddI32, SubtractI32, MultiplyI32, DivideI32,
  RemainderI32, EqualI32, NotEqualI32, LessThanI32, LessThanOrEqualI32,
  GreaterThanI32, GreaterThanOrEqualI32, Entry
}
use Syntax.Ast.{IntegerLiteral}

let c_integer_operation = fn operator ->
  match operator {
    | AddI32 -> "rt_i32_add"
    | SubtractI32 -> "rt_i32_sub"
    | MultiplyI32 -> "rt_i32_mul"
    | DivideI32 -> "rt_i32_div"
    | RemainderI32 -> "rt_i32_rem"
    | EqualI32 -> "rt_i32_equal"
    | NotEqualI32 -> "rt_i32_not_equal"
    | LessThanI32 -> "rt_i32_less"
    | LessThanOrEqualI32 -> "rt_i32_less_equal"
    | GreaterThanI32 -> "rt_i32_greater"
    | GreaterThanOrEqualI32 -> "rt_i32_greater_equal"
  }

let emit_binary = fn emit_child -> fn (LambdaBinaryExpression {
  operator,
  left,
  right
}) ->
  c_integer_operation operator + "(" + emit_child left + "," + emit_child right + ")"

let emit_conditional = fn emit_child -> fn (LambdaConditionalExpression {
  condition,
  when_true,
  when_false
}) ->
  "(rt_as_bool(" + emit_child condition + ") ? "
  + emit_child when_true + " : " + emit_child when_false + ")"

let emit_expression = fn expression ->
  match expression {
    | LambdaInteger(IntegerLiteral(value)) -> "rt_int(" + value + ")"
    | LambdaBoolean(true) -> "rt_bool(1)"
    | LambdaBoolean(false) -> "rt_bool(0)"
    | LambdaBinary(binary) -> emit_binary emit_expression binary
    | LambdaConditional(conditional) -> emit_conditional emit_expression conditional
    | LambdaNegation(operand) -> "rt_i32_neg(" + emit_expression operand + ")"
  }

let emit_entry_function = fn expression ->
  "#include \"runtime.h\"\n"
  + "RtValue rt_riotml_entry(RtValue environment, RtValue argument) {\n"
  + "  (void)environment; (void)argument;\n  return " + emit_expression expression + ";\n}\n"

let emit_process_entry = fn () ->
  "int main(int argc, char **argv) {\n"
  + "  rt_set_args(argc, argv);\n"
  + "  RtValue entry = rt_function(rt_riotml_entry, rt_unit());\n"
  + "  return (int)rt_as_int(rt_apply(entry, rt_unit()));\n}\n"

let emit_program = fn (Entry(expression)) ->
  emit_entry_function expression + emit_process_entry ()
