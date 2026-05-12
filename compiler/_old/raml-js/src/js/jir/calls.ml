open Std
module Core = Raml_core.Core_ir
module Jir = Types
module Builtins = Builtins
module Intrinsics = Intrinsics
module Primitives = Primitives
module References = References

let bool_literal = fun value -> Jir.Expr.Literal (Jir.Literal.Bool value)

let direct_callee = fun entity_id -> References.entity entity_id

let lower_builtin = fun entity_id builtin arguments ->
  let fallback () = Intrinsics.call (direct_callee entity_id) arguments in
  match (builtin: Builtins.direct_callee) with
  | Console_log -> (
      match arguments with
      | [ argument ] -> Intrinsics.console_log [ argument ]
      | _ -> fallback ()
    )
  | Console_error -> (
      match arguments with
      | [ argument ] -> Intrinsics.console_error [ argument ]
      | _ -> fallback ()
    )
  | Stdout_write -> (
      match arguments with
      | [ argument ] -> Intrinsics.stdout_write argument
      | _ -> fallback ()
    )
  | Stderr_write -> (
      match arguments with
      | [ argument ] -> Intrinsics.stderr_write argument
      | _ -> fallback ()
    )
  | String_constructor -> (
      match arguments with
      | [ argument ] -> Intrinsics.string_constructor argument
      | _ -> fallback ()
    )
  | Math_sqrt -> (
      match arguments with
      | [ argument ] -> Intrinsics.math_sqrt argument
      | _ -> fallback ()
    )
  | Primitive primitive_name ->
      Primitives.lower primitive_name arguments
  | Unary_operator operator -> (
      match arguments with
      | [ argument ] -> Intrinsics.unary operator argument
      | _ -> fallback ()
    )
  | Boolean_and -> (
      match arguments with
      | [left;right] -> Jir.Expr.Conditional Jir.Expr.{
        condition = left;
        then_ = right;
        else_ = bool_literal false
      }
      | _ -> fallback ()
    )
  | Boolean_or -> (
      match arguments with
      | [left;right] -> Jir.Expr.Conditional Jir.Expr.{
        condition = left;
        then_ = bool_literal true;
        else_ = right
      }
      | _ -> fallback ()
    )
  | Binary_operator operator -> (
      match arguments with
      | [left;right] -> Intrinsics.binary operator left right
      | _ -> fallback ()
    )

let direct = fun entity_id arguments ->
  match Builtins.classify_direct_callee entity_id with
  | Some builtin -> lower_builtin entity_id builtin arguments
  | None -> Intrinsics.call (direct_callee entity_id) arguments
