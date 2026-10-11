// Type checking for the initial native RiotML entry-point language.
use Prelude.{Result, Ok, Err}
use Syntax.Ast.{Expr, Program, ValueType, I32, BoolType, Integer, Boolean, Add, Subtract, Multiply, Divide, Negate, Main}

let infer_expr = fn expression ->
  match expression {
    | Integer(_) -> Ok(I32)
    | Boolean(_) -> Ok(BoolType)
    | Add(left, right) -> match (infer_expr left) {
      | Err(message) -> Err(message)
      | Ok(left_type) -> match (infer_expr right) {
        | Err(message) -> Err(message)
        | Ok(right_type) -> match left_type {
          | I32 -> match right_type {
            | I32 -> Ok(I32)
            | _ -> Err("`+` requires two i32 operands")
          }
          | _ -> Err("`+` requires two i32 operands")
        }
      }
    }
    | Subtract(left, right) -> match (infer_expr left) {
      | Err(message) -> Err(message)
      | Ok(left_type) -> match (infer_expr right) {
        | Err(message) -> Err(message)
        | Ok(right_type) -> match left_type {
          | I32 -> match right_type {
            | I32 -> Ok(I32)
            | _ -> Err("`-` requires two i32 operands")
          }
          | _ -> Err("`-` requires two i32 operands")
        }
      }
    }
    | Multiply(left, right) -> match (infer_expr left) {
      | Err(message) -> Err(message)
      | Ok(left_type) -> match (infer_expr right) {
        | Err(message) -> Err(message)
        | Ok(right_type) -> match left_type {
          | I32 -> match right_type {
            | I32 -> Ok(I32)
            | _ -> Err("`*` requires two i32 operands")
          }
          | _ -> Err("`*` requires two i32 operands")
        }
      }
    }
    | Divide(left, right) -> match (infer_expr left) {
      | Err(message) -> Err(message)
      | Ok(left_type) -> match (infer_expr right) {
        | Err(message) -> Err(message)
        | Ok(right_type) -> match left_type {
          | I32 -> match right_type {
            | I32 -> Ok(I32)
            | _ -> Err("`/` requires two i32 operands")
          }
          | _ -> Err("`/` requires two i32 operands")
        }
      }
    }
    | Negate(operand) -> match (infer_expr operand) {
      | Err(message) -> Err(message)
      | Ok(I32) -> Ok(I32)
      | Ok(_) -> Err("unary `-` requires an i32 operand")
    }
  }

let check_program = fn program ->
  match program {
    | Main(expression) -> match (infer_expr expression) {
      | Ok(I32) -> Ok(program)
      | Ok(_) -> Err("native entry point must return i32")
      | Err(message) -> Err(message)
    }
  }
