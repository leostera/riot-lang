#!/bin/bash
# Create more type tests
cat > 0506_type_polymorphic.ml << 'EOF'
type 'a list = Nil | Cons of 'a * 'a list
EOF

cat > 0507_type_alias.ml << 'EOF'
type myint = int
EOF

cat > 0508_type_arrow_chain.ml << 'EOF'
type t = int -> int -> int
EOF

cat > 0509_type_tuple_three.ml << 'EOF'
type triple = int * string * bool
EOF

cat > 0510_type_variant_multi.ml << 'EOF'
type color = Red | Green | Blue | RGB of int * int * int
EOF

# Create expression edge case tests
cat > 0511_nested_app.ml << 'EOF'
let x = f (g (h i))
EOF

cat > 0512_complex_infix.ml << 'EOF'
let x = 1 + 2 * 3 - 4
EOF

cat > 0513_nested_if.ml << 'EOF'
let x = if a then if b then 1 else 2 else 3
EOF

cat > 0514_list_of_tuples.ml << 'EOF'
let x = [(1, 2); (3, 4); (5, 6)]
EOF

cat > 0515_tuple_of_lists.ml << 'EOF'
let x = ([1; 2], [3; 4])
EOF

cat > 0516_nested_records.ml << 'EOF'
let x = { a = { b = 1 } }
EOF

cat > 0517_record_with_list.ml << 'EOF'
let x = { items = [1; 2; 3] }
EOF

cat > 0518_match_nested_tuple.ml << 'EOF'
let x = match y with (a, (b, c)) -> a + b + c
EOF

cat > 0519_fun_returning_fun.ml << 'EOF'
let x = fun a -> fun b -> a + b
EOF

cat > 0520_complex_cons.ml << 'EOF'
let x = 1 :: 2 :: 3 :: []
EOF

echo "Created 20 new test files"
