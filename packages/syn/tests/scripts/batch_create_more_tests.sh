#!/bin/bash
# Create more expression tests
for i in {521..540}; do
  case $i in
    521) cat > 0${i}_deeply_nested_let.ml << 'EOF'
let a = 1 in let b = 2 in let c = 3 in a + b + c
EOF
      ;;
    522) cat > 0${i}_match_with_guards.ml << 'EOF'
let x = match y with | n when n > 0 -> 1 | _ -> 0
EOF
      ;;
    523) cat > 0${i}_nested_match.ml << 'EOF'
let x = match a with | Some x -> (match x with | 1 -> true | _ -> false) | None -> false
EOF
      ;;
    524) cat > 0${i}_array_literal.ml << 'EOF'
let x = [| 1; 2; 3; 4; 5 |]
EOF
      ;;
    525) cat > 0${i}_array_nested.ml << 'EOF'
let x = [| [| 1; 2 |]; [| 3; 4 |] |]
EOF
      ;;
    526) cat > 0${i}_ref_operations.ml << 'EOF'
let x = ref 0 in x := !x + 1
EOF
      ;;
    527) cat > 0${i}_sequence_three.ml << 'EOF'
let x = (print 1; print 2; print 3)
EOF
      ;;
    528) cat > 0${i}_nested_seq.ml << 'EOF'
let x = ((a; b); (c; d))
EOF
      ;;
    529) cat > 0${i}_tuple_large.ml << 'EOF'
let x = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
EOF
      ;;
    530) cat > 0${i}_list_large.ml << 'EOF'
let x = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
EOF
      ;;
    531) cat > 0${i}_record_complex.ml << 'EOF'
let x = { a = 1; b = "test"; c = true; d = [1; 2; 3] }
EOF
      ;;
    532) cat > 0${i}_record_update_nested.ml << 'EOF'
let x = { r with a = { r.a with b = 1 } }
EOF
      ;;
    533) cat > 0${i}_field_access_chain.ml << 'EOF'
let x = obj.a.b.c.d
EOF
      ;;
    534) cat > 0${i}_mixed_operators.ml << 'EOF'
let x = a + b * c - d / e
EOF
      ;;
    535) cat > 0${i}_operator_precedence.ml << 'EOF'
let x = 1 + 2 * 3 + 4 * 5 + 6
EOF
      ;;
    536) cat > 0${i}_bool_operators.ml << 'EOF'
let x = a && b || c && d
EOF
      ;;
    537) cat > 0${i}_comparison_chain.ml << 'EOF'
let x = a < b && b < c && c < d
EOF
      ;;
    538) cat > 0${i}_cons_operator.ml << 'EOF'
let x = a :: b :: c :: d :: []
EOF
      ;;
    539) cat > 0${i}_string_concat.ml << 'EOF'
let x = "hello" ^ " " ^ "world"
EOF
      ;;
    540) cat > 0${i}_list_append.ml << 'EOF'
let x = [1; 2] @ [3; 4] @ [5; 6]
EOF
      ;;
  esac
done

# Create pattern tests
for i in {541..560}; do
  case $i in
    541) cat > 0${i}_pattern_tuple_nested.ml << 'EOF'
let ((a, b), (c, d)) = x
EOF
      ;;
    542) cat > 0${i}_pattern_list_cons.ml << 'EOF'
let (x :: y :: rest) = list
EOF
      ;;
    543) cat > 0${i}_pattern_record_nested.ml << 'EOF'
let { a = { b = x } } = record
EOF
      ;;
    544) cat > 0${i}_pattern_constructor.ml << 'EOF'
let Some (x, y) = option
EOF
      ;;
    545) cat > 0${i}_pattern_poly_variant.ml << 'EOF'
let `Tag x = variant
EOF
      ;;
    546) cat > 0${i}_match_all_patterns.ml << 'EOF'
let x = match y with | [] -> 0 | [a] -> a | a :: b :: _ -> a + b
EOF
      ;;
    547) cat > 0${i}_function_pattern.ml << 'EOF'
let f = function | 0 -> "zero" | 1 -> "one" | _ -> "other"
EOF
      ;;
    548) cat > 0${i}_fun_tuple_param.ml << 'EOF'
let f = fun (a, b) -> a + b
EOF
      ;;
    549) cat > 0${i}_fun_list_param.ml << 'EOF'
let f = fun [x; y] -> x + y
EOF
      ;;
    550) cat > 0${i}_fun_record_param.ml << 'EOF'
let f = fun { x; y } -> x + y
EOF
      ;;
    551) cat > 0${i}_nested_fun.ml << 'EOF'
let f = fun a -> fun b -> fun c -> a + b + c
EOF
      ;;
    552) cat > 0${i}_fun_with_match.ml << 'EOF'
let f = fun x -> match x with | Some y -> y | None -> 0
EOF
      ;;
    553) cat > 0${i}_complex_lambda.ml << 'EOF'
let f = fun x -> if x > 0 then x * 2 else x * 3
EOF
      ;;
    554) cat > 0${i}_labeled_args.ml << 'EOF'
let x = f ~a:1 ~b:2
EOF
      ;;
    555) cat > 0${i}_optional_args.ml << 'EOF'
let x = f ?a:None ?b:(Some 2)
EOF
      ;;
    556) cat > 0${i}_mixed_args.ml << 'EOF'
let x = f ~a:1 ?b:None 3
EOF
      ;;
    557) cat > 0${i}_label_punning.ml << 'EOF'
let x = f ~a ~b
EOF
      ;;
    558) cat > 0${i}_poly_variant_match.ml << 'EOF'
let x = match y with | `A -> 1 | `B x -> x | _ -> 0
EOF
      ;;
    559) cat > 0${i}_lazy_expr.ml << 'EOF'
let x = lazy (expensive_computation ())
EOF
      ;;
    560) cat > 0${i}_assert_expr.ml << 'EOF'
let x = assert (y > 0)
EOF
      ;;
  esac
done

# Create type tests
for i in {561..580}; do
  case $i in
    561) cat > 0${i}_type_list.ml << 'EOF'
type 'a mylist = 'a list
EOF
      ;;
    562) cat > 0${i}_type_option.ml << 'EOF'
type 'a myoption = 'a option
EOF
      ;;
    563) cat > 0${i}_type_result.ml << 'EOF'
type ('a, 'b) result = Ok of 'a | Error of 'b
EOF
      ;;
    564) cat > 0${i}_type_tree.ml << 'EOF'
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
EOF
      ;;
    565) cat > 0${i}_type_record_poly.ml << 'EOF'
type 'a point = { x: 'a; y: 'a }
EOF
      ;;
    566) cat > 0${i}_type_nested_variant.ml << 'EOF'
type t = A of int | B of (string * bool)
EOF
      ;;
    567) cat > 0${i}_type_recursive.ml << 'EOF'
type expr = Const of int | Add of expr * expr
EOF
      ;;
    568) cat > 0${i}_type_tuple_variant.ml << 'EOF'
type coord = Point of int * int | Origin
EOF
      ;;
    569) cat > 0${i}_type_record_variant.ml << 'EOF'
type shape = Circle of { radius: float } | Rectangle of { width: float; height: float }
EOF
      ;;
    570) cat > 0${i}_type_poly_two.ml << 'EOF'
type ('a, 'b) pair = 'a * 'b
EOF
      ;;
    571) cat > 0${i}_type_arrow_multi.ml << 'EOF'
type t = int -> string -> bool -> unit
EOF
      ;;
    572) cat > 0${i}_type_tuple_complex.ml << 'EOF'
type t = int * string * bool * float
EOF
      ;;
    573) cat > 0${i}_type_nested_tuple.ml << 'EOF'
type t = (int * int) * (string * string)
EOF
      ;;
    574) cat > 0${i}_type_list_tuple.ml << 'EOF'
type t = (int * string) list
EOF
      ;;
    575) cat > 0${i}_type_option_list.ml << 'EOF'
type t = int list option
EOF
      ;;
    576) cat > 0${i}_type_function_tuple.ml << 'EOF'
type t = (int * int) -> int
EOF
      ;;
    577) cat > 0${i}_type_curried.ml << 'EOF'
type t = int -> (int -> int)
EOF
      ;;
    578) cat > 0${i}_type_record_mutable.ml << 'EOF'
type t = { mutable x: int; y: string }
EOF
      ;;
    579) cat > 0${i}_type_variant_many.ml << 'EOF'
type t = A | B | C | D | E | F | G | H
EOF
      ;;
    580) cat > 0${i}_type_nested_record.ml << 'EOF'
type t = { a: { b: int; c: string }; d: bool }
EOF
      ;;
  esac
done

# Create operator precedence tests
for i in {581..600}; do
  case $i in
    581) cat > 0${i}_prec_add_mul.ml << 'EOF'
let x = 1 + 2 * 3
EOF
      ;;
    582) cat > 0${i}_prec_mul_div.ml << 'EOF'
let x = 10 / 2 * 3
EOF
      ;;
    583) cat > 0${i}_prec_paren.ml << 'EOF'
let x = (1 + 2) * 3
EOF
      ;;
    584) cat > 0${i}_prec_comp_bool.ml << 'EOF'
let x = a < b && c > d
EOF
      ;;
    585) cat > 0${i}_prec_cons_append.ml << 'EOF'
let x = 1 :: [2] @ [3]
EOF
      ;;
    586) cat > 0${i}_prec_neg_mul.ml << 'EOF'
let x = -2 * 3
EOF
      ;;
    587) cat > 0${i}_prec_app_infix.ml << 'EOF'
let x = f a + g b
EOF
      ;;
    588) cat > 0${i}_prec_tuple_app.ml << 'EOF'
let x = (f a, g b)
EOF
      ;;
    589) cat > 0${i}_prec_if_seq.ml << 'EOF'
let x = if a then b; c else d
EOF
      ;;
    590) cat > 0${i}_prec_let_seq.ml << 'EOF'
let x = let y = 1 in y; 2
EOF
      ;;
    591) cat > 0${i}_prec_match_app.ml << 'EOF'
let x = match f a with | Some x -> x | None -> 0
EOF
      ;;
    592) cat > 0${i}_prec_fun_app.ml << 'EOF'
let x = fun y -> f y
EOF
      ;;
    593) cat > 0${i}_prec_ref_deref.ml << 'EOF'
let x = !r + 1
EOF
      ;;
    594) cat > 0${i}_prec_field_app.ml << 'EOF'
let x = obj.field arg
EOF
      ;;
    595) cat > 0${i}_prec_index_app.ml << 'EOF'
let x = arr.(i) + 1
EOF
      ;;
    596) cat > 0${i}_prec_string_concat.ml << 'EOF'
let x = "a" ^ "b" ^ "c"
EOF
      ;;
    597) cat > 0${i}_prec_list_ops.ml << 'EOF'
let x = [1] @ [2] @ [3]
EOF
      ;;
    598) cat > 0${i}_prec_bool_short.ml << 'EOF'
let x = a || b && c
EOF
      ;;
    599) cat > 0${i}_prec_comp_chain.ml << 'EOF'
let x = a = b && b = c
EOF
      ;;
    600) cat > 0${i}_prec_mixed_all.ml << 'EOF'
let x = a + b * c - d / e mod f
EOF
      ;;
  esac
done

echo "Created 80 more test files (521-600)"
