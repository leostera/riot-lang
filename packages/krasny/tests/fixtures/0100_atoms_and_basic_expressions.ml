(* TODO(@leostera): we need to add more examples here for:
  - [x] Large int values in all bases (10, 8, 2, 16)
  - [x] Multiline strings
  - [x] Large floats
  - [x] Very large module paths (think from 2 to 100 module identifier paths parts)
*)

let int_negative = -42
let int_decimal_large = 9_223_372_036
let int_octal_large = 0o7_123_456
let int_binary_large = 0b1010_0101_1111_0000
let int_hex = 0xff
let int_hex_large = 0xDEAD_BEEF
let float_negative = -3.14
let float_large = 123_456_789.987_654
let string_literal = "hello"
let string_multiline = {|
line one
line two
line three
|}
let char_literal = 'x'
let bool_true = true
let bool_false = false
let unit_literal = ()
let ident_simple = value
let ident_underscore = _value
let ident_prime = value'
let ident_caps = Value
let ident_digits = value2
let path_expr = Module.Submodule.value
let path_expr_large = Alpha.Bravo.Charlie.Delta.Echo.Foxtrot.Golf.Hotel.India.Juliet.Kilo.value
let apply_simple = f x
let apply_nested = f (g x)
let apply_multiple = f x y z
let _ignored = true
