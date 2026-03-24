let paren_simple = (value)
let paren_nested = ((value))
let paren_string = ("hello")
let paren_bool = (flag)
let prefix_neg = -x
let prefix_not = not value
let add_two_ints = 1 + 2
let sub_two_ints = 5 - 3
let mul_two_ints = 2 * 3
let div_two_ints = 8 / 2
let mod_two_ints = 5 mod 2
let add_chain = 1 + 2 + 3
let mul_precedence = 1 + 2 * 3
let paren_precedence = (1 + 2) * 3
let mixed_ops = 1 + 2 * 3 - 4 / 2
let float_add = 1.0 +. 2.0
let eq = a = b
let neq = a <> b
let lt = a < b
let gt = a > b
let lte = a <= b
let gte = a >= b
let string_eq = left = right
let bool_eq = left = right
let comparison_chain = a < b && b < c
let nested_comparison = (a + b) > (c + d)
let and_chain = a && b && c
let or_chain = a || b || c
let mixed_logic = a && b || c
