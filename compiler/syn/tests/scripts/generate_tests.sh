#!/bin/bash

# Literals - 07-20
cat > 07_int_decimal.ml << 'ML'
let x = 42
ML

cat > 08_int_negative.ml << 'ML'
let x = -10
ML

cat > 09_int_hex.ml << 'ML'
let x = 0xFF
ML

cat > 10_int_octal.ml << 'ML'
let x = 0o777
ML

cat > 11_int_binary.ml << 'ML'
let x = 0b1010
ML

cat > 12_float_simple.ml << 'ML'
let x = 3.14
ML

cat > 13_float_negative.ml << 'ML'
let x = -2.5
ML

cat > 14_float_exponent.ml << 'ML'
let x = 1.5e10
ML

cat > 15_float_neg_exponent.ml << 'ML'
let x = 2.0e-5
ML

cat > 16_string_empty.ml << 'ML'
let x = ""
ML

cat > 17_string_simple.ml << 'ML'
let x = "hello world"
ML

cat > 18_char_simple.ml << 'ML'
let x = 'a'
ML

cat > 19_bool_false.ml << 'ML'
let x = false
ML

cat > 20_unit.ml << 'ML'
let x = ()
ML

# Identifiers - 21-25
cat > 21_ident_simple.ml << 'ML'
let x = y
ML

cat > 22_ident_underscore.ml << 'ML'
let x = my_var
ML

cat > 23_ident_prime.ml << 'ML'
let x = x'
ML

cat > 24_ident_caps.ml << 'ML'
let x = MyModule
ML

cat > 25_ident_digits.ml << 'ML'
let x = var123
ML

# Parenthesized expressions - 26-30
cat > 26_paren_simple.ml << 'ML'
let x = (1)
ML

cat > 27_paren_nested.ml << 'ML'
let x = ((42))
ML

cat > 28_paren_ident.ml << 'ML'
let x = (y)
ML

cat > 29_paren_string.ml << 'ML'
let x = ("hello")
ML

cat > 30_paren_bool.ml << 'ML'
let x = (true)
ML

# Infix operators - arithmetic - 31-40
cat > 31_add_two_ints.ml << 'ML'
let x = 1 + 2
ML

cat > 32_sub_two_ints.ml << 'ML'
let x = 5 - 3
ML

cat > 33_mul_two_ints.ml << 'ML'
let x = 4 * 3
ML

cat > 34_div_two_ints.ml << 'ML'
let x = 10 / 2
ML

cat > 35_mod_two_ints.ml << 'ML'
let x = 10 mod 3
ML

cat > 36_add_chain.ml << 'ML'
let x = 1 + 2 + 3
ML

cat > 37_mul_precedence.ml << 'ML'
let x = 1 + 2 * 3
ML

cat > 38_paren_precedence.ml << 'ML'
let x = (1 + 2) * 3
ML

cat > 39_mixed_ops.ml << 'ML'
let x = 1 + 2 - 3 * 4 / 5
ML

cat > 40_float_add.ml << 'ML'
let x = 1.5 +. 2.5
ML

# Comparison operators - 41-50
cat > 41_eq.ml << 'ML'
let x = 1 = 2
ML

cat > 42_neq.ml << 'ML'
let x = 1 <> 2
ML

cat > 43_lt.ml << 'ML'
let x = 1 < 2
ML

cat > 44_gt.ml << 'ML'
let x = 1 > 2
ML

cat > 45_lte.ml << 'ML'
let x = 1 <= 2
ML

cat > 46_gte.ml << 'ML'
let x = 1 >= 2
ML

cat > 47_string_eq.ml << 'ML'
let x = "a" = "b"
ML

cat > 48_bool_eq.ml << 'ML'
let x = true = false
ML

cat > 49_comparison_chain.ml << 'ML'
let x = 1 < 2 && 2 < 3
ML

cat > 50_nested_comparison.ml << 'ML'
let x = (1 + 2) < (3 + 4)
ML

# Logical operators - 51-55
cat > 51_and.ml << 'ML'
let x = true && false
ML

cat > 52_or.ml << 'ML'
let x = true || false
ML

cat > 53_and_chain.ml << 'ML'
let x = true && false && true
ML

cat > 54_or_chain.ml << 'ML'
let x = false || false || true
ML

cat > 55_mixed_logic.ml << 'ML'
let x = true && false || true
ML

# Prefix operators - 56-60
cat > 56_neg_int.ml << 'ML'
let x = -42
ML

cat > 57_neg_float.ml << 'ML'
let x = -.3.14
ML

cat > 58_not.ml << 'ML'
let x = not true
ML

cat > 59_neg_expr.ml << 'ML'
let x = -(1 + 2)
ML

cat > 60_not_comparison.ml << 'ML'
let x = not (1 = 2)
ML

# Function application - 61-70
cat > 61_app_one_arg.ml << 'ML'
let x = f 1
ML

cat > 62_app_two_args.ml << 'ML'
let x = f 1 2
ML

cat > 63_app_three_args.ml << 'ML'
let x = f 1 2 3
ML

cat > 64_app_with_paren.ml << 'ML'
let x = f (1 + 2)
ML

cat > 65_app_nested.ml << 'ML'
let x = f (g 1)
ML

cat > 66_app_chain.ml << 'ML'
let x = f g h
ML

cat > 67_app_infix_result.ml << 'ML'
let x = f 1 + 2
ML

cat > 68_app_module_path.ml << 'ML'
let x = List.map f xs
ML

cat > 69_app_constructor.ml << 'ML'
let x = Some 42
ML

cat > 70_app_unit.ml << 'ML'
let x = print ()
ML

# If expressions - 71-80
cat > 71_if_then_else_simple.ml << 'ML'
let x = if true then 1 else 2
ML

cat > 72_if_condition_expr.ml << 'ML'
let x = if 1 < 2 then "yes" else "no"
ML

cat > 73_if_nested.ml << 'ML'
let x = if true then if false then 1 else 2 else 3
ML

cat > 74_if_complex_branches.ml << 'ML'
let x = if true then 1 + 2 else 3 * 4
ML

cat > 75_if_paren_condition.ml << 'ML'
let x = if (a && b) then 1 else 0
ML

cat > 76_if_multiline.ml << 'ML'
let x = 
  if true then
    1
  else
    2
ML

cat > 77_if_no_else.ml << 'ML'
let x = if true then ()
ML

cat > 78_if_function_call.ml << 'ML'
let x = if test x then f 1 else g 2
ML

cat > 79_if_string_result.ml << 'ML'
let x = if flag then "on" else "off"
ML

cat > 80_if_bool_result.ml << 'ML'
let x = if a then true else false
ML

# Let expressions - 81-90
cat > 81_let_in_simple.ml << 'ML'
let x = let y = 1 in y
ML

cat > 82_let_in_two_bindings.ml << 'ML'
let x = let y = 1 in let z = 2 in y + z
ML

cat > 83_let_in_use_outer.ml << 'ML'
let x = 10 in let y = x + 1 in y
ML

cat > 84_let_in_nested.ml << 'ML'
let x = let y = let z = 1 in z + 1 in y + 1
ML

cat > 85_let_in_if.ml << 'ML'
let x = let y = 1 in if y > 0 then y else 0
ML

cat > 86_let_in_function.ml << 'ML'
let x = let f = fun x -> x + 1 in f 10
ML

cat > 87_let_in_complex_expr.ml << 'ML'
let x = let y = 1 + 2 in let z = y * 3 in z - 1
ML

cat > 88_let_rec_in.ml << 'ML'
let x = let rec f n = if n = 0 then 1 else n * f (n - 1) in f 5
ML

cat > 89_let_in_multiline.ml << 'ML'
let x =
  let y = 1 in
  let z = 2 in
  y + z
ML

cat > 90_let_and_bindings.ml << 'ML'
let x = let a = 1 and b = 2 in a + b
ML

# Match expressions - 91-100
cat > 91_match_simple.ml << 'ML'
let x = match 1 with | 1 -> true | _ -> false
ML

cat > 92_match_two_cases.ml << 'ML'
let x = match n with | 0 -> "zero" | 1 -> "one"
ML

cat > 93_match_wildcard.ml << 'ML'
let x = match value with | _ -> 42
ML

cat > 94_match_variable.ml << 'ML'
let x = match y with | n -> n + 1
ML

cat > 95_match_or_pattern.ml << 'ML'
let x = match n with | 1 | 2 | 3 -> true | _ -> false
ML

cat > 96_match_tuple.ml << 'ML'
let x = match pair with | (a, b) -> a + b
ML

cat > 97_match_list.ml << 'ML'
let x = match lst with | [] -> 0 | x :: xs -> x
ML

cat > 98_match_constructor.ml << 'ML'
let x = match opt with | Some n -> n | None -> 0
ML

cat > 99_match_nested.ml << 'ML'
let x = match a with | Some (x, y) -> x + y | None -> 0
ML

cat > 100_match_when_guard.ml << 'ML'
let x = match n with | x when x > 0 -> x | _ -> 0
ML

echo "Generated 100 test files"
