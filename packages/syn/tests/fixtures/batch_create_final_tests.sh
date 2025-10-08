#!/bin/bash
# Create final batch of tests (601-700) - 100 more tests!

# More complex expressions (601-630)
for i in {601..630}; do
  case $i in
    601) cat > 0${i}_begin_end.ml << 'EOF'
let x = begin 1 + 2 end
EOF
      ;;
    602) cat > 0${i}_begin_seq.ml << 'EOF'
let x = begin print 1; print 2; 3 end
EOF
      ;;
    603) cat > 0${i}_nested_begin.ml << 'EOF'
let x = begin begin 1 end end
EOF
      ;;
    604) cat > 0${i}_for_loop.ml << 'EOF'
let x = for i = 1 to 10 do print i done
EOF
      ;;
    605) cat > 0${i}_for_downto.ml << 'EOF'
let x = for i = 10 downto 1 do print i done
EOF
      ;;
    606) cat > 0${i}_while_loop.ml << 'EOF'
let x = while true do print "loop" done
EOF
      ;;
    607) cat > 0${i}_try_with.ml << 'EOF'
let x = try risky () with | Failure msg -> 0
EOF
      ;;
    608) cat > 0${i}_try_multi_handlers.ml << 'EOF'
let x = try f () with | Not_found -> 0 | Failure _ -> 1
EOF
      ;;
    609) cat > 0${i}_let_rec_simple.ml << 'EOF'
let rec f x = if x = 0 then 1 else x * f (x - 1)
EOF
      ;;
    610) cat > 0${i}_let_rec_and.ml << 'EOF'
let rec even x = if x = 0 then true else odd (x - 1) and odd x = if x = 0 then false else even (x - 1)
EOF
      ;;
    611) cat > 0${i}_match_exception.ml << 'EOF'
let x = match y with | exception Not_found -> None | v -> Some v
EOF
      ;;
    612) cat > 0${i}_fun_labeled.ml << 'EOF'
let f = fun ~x ~y -> x + y
EOF
      ;;
    613) cat > 0${i}_fun_optional.ml << 'EOF'
let f = fun ?x ?y () -> 0
EOF
      ;;
    614) cat > 0${i}_fun_labeled_optional.ml << 'EOF'
let f = fun ~x ?y () -> x
EOF
      ;;
    615) cat > 0${i}_constructor_app.ml << 'EOF'
let x = Some 42
EOF
      ;;
    616) cat > 0${i}_constructor_multi.ml << 'EOF'
let x = Result.Ok (Some 42)
EOF
      ;;
    617) cat > 0${i}_poly_var_simple.ml << 'EOF'
let x = `Red
EOF
      ;;
    618) cat > 0${i}_poly_var_arg.ml << 'EOF'
let x = `RGB (255, 0, 0)
EOF
      ;;
    619) cat > 0${i}_record_single.ml << 'EOF'
let x = { name = "Alice" }
EOF
      ;;
    620) cat > 0${i}_record_multiple.ml << 'EOF'
let x = { name = "Bob"; age = 30; active = true }
EOF
      ;;
    621) cat > 0${i}_record_update_simple.ml << 'EOF'
let x = { person with age = 31 }
EOF
      ;;
    622) cat > 0${i}_record_update_multi.ml << 'EOF'
let x = { person with age = 31; active = false }
EOF
      ;;
    623) cat > 0${i}_array_get.ml << 'EOF'
let x = arr.(0)
EOF
      ;;
    624) cat > 0${i}_array_set.ml << 'EOF'
let x = arr.(0) <- 42
EOF
      ;;
    625) cat > 0${i}_string_get.ml << 'EOF'
let x = str.[0]
EOF
      ;;
    626) cat > 0${i}_string_set.ml << 'EOF'
let x = str.[0] <- 'a'
EOF
      ;;
    627) cat > 0${i}_method_call.ml << 'EOF'
let x = obj#method_name
EOF
      ;;
    628) cat > 0${i}_method_call_arg.ml << 'EOF'
let x = obj#method_name 42
EOF
      ;;
    629) cat > 0${i}_coerce.ml << 'EOF'
let x = (obj :> base_type)
EOF
      ;;
    630) cat > 0${i}_double_coerce.ml << 'EOF'
let x = (obj : specific_type :> base_type)
EOF
      ;;
  esac
done

# More pattern matching tests (631-660)
for i in {631..660}; do
  case $i in
    631) cat > 0${i}_match_int.ml << 'EOF'
let x = match n with | 0 -> "zero" | 1 -> "one" | _ -> "many"
EOF
      ;;
    632) cat > 0${i}_match_string.ml << 'EOF'
let x = match s with | "yes" -> true | "no" -> false | _ -> false
EOF
      ;;
    633) cat > 0${i}_match_bool.ml << 'EOF'
let x = match b with | true -> 1 | false -> 0
EOF
      ;;
    634) cat > 0${i}_match_tuple_simple.ml << 'EOF'
let x = match pair with | (0, 0) -> "origin" | _ -> "point"
EOF
      ;;
    635) cat > 0${i}_match_list_empty.ml << 'EOF'
let x = match lst with | [] -> 0 | _ -> 1
EOF
      ;;
    636) cat > 0${i}_match_list_one.ml << 'EOF'
let x = match lst with | [x] -> x | _ -> 0
EOF
      ;;
    637) cat > 0${i}_match_list_two.ml << 'EOF'
let x = match lst with | [x; y] -> x + y | _ -> 0
EOF
      ;;
    638) cat > 0${i}_match_cons_simple.ml << 'EOF'
let x = match lst with | h :: t -> h | [] -> 0
EOF
      ;;
    639) cat > 0${i}_match_cons_multi.ml << 'EOF'
let x = match lst with | a :: b :: rest -> a + b | _ -> 0
EOF
      ;;
    640) cat > 0${i}_match_option_none.ml << 'EOF'
let x = match opt with | None -> 0 | Some v -> v
EOF
      ;;
    641) cat > 0${i}_match_option_some.ml << 'EOF'
let x = match opt with | Some (Some x) -> x | _ -> 0
EOF
      ;;
    642) cat > 0${i}_match_result.ml << 'EOF'
let x = match res with | Ok v -> v | Error _ -> 0
EOF
      ;;
    643) cat > 0${i}_match_record_simple.ml << 'EOF'
let x = match r with | { x = 0 } -> true | _ -> false
EOF
      ;;
    644) cat > 0${i}_match_record_multi.ml << 'EOF'
let x = match r with | { x = 0; y = 0 } -> "origin" | _ -> "point"
EOF
      ;;
    645) cat > 0${i}_match_wildcard_only.ml << 'EOF'
let x = match y with | _ -> 42
EOF
      ;;
    646) cat > 0${i}_match_var_bind.ml << 'EOF'
let x = match y with | v -> v + 1
EOF
      ;;
    647) cat > 0${i}_match_guard_simple.ml << 'EOF'
let x = match n with | x when x > 0 -> x | _ -> 0
EOF
      ;;
    648) cat > 0${i}_match_guard_complex.ml << 'EOF'
let x = match n with | x when x > 0 && x < 10 -> x | _ -> 0
EOF
      ;;
    649) cat > 0${i}_match_nested_cons.ml << 'EOF'
let x = match lst with | (a :: b) :: rest -> a | _ -> 0
EOF
      ;;
    650) cat > 0${i}_match_tuple_cons.ml << 'EOF'
let x = match pair with | (h :: t, x) -> h + x | _ -> 0
EOF
      ;;
    651) cat > 0${i}_function_simple.ml << 'EOF'
let f = function | 0 -> "zero" | _ -> "other"
EOF
      ;;
    652) cat > 0${i}_function_multi.ml << 'EOF'
let f = function | [] -> 0 | [x] -> x | x :: _ -> x
EOF
      ;;
    653) cat > 0${i}_function_guard.ml << 'EOF'
let f = function | x when x > 0 -> x | _ -> 0
EOF
      ;;
    654) cat > 0${i}_function_nested.ml << 'EOF'
let f = function | Some (Some x) -> x | _ -> 0
EOF
      ;;
    655) cat > 0${i}_match_array_literal.ml << 'EOF'
let x = match arr with | [||] -> 0 | _ -> 1
EOF
      ;;
    656) cat > 0${i}_match_string_literal.ml << 'EOF'
let x = match s with | "" -> 0 | _ -> 1
EOF
      ;;
    657) cat > 0${i}_match_char.ml << 'EOF'
let x = match c with | 'a' -> 1 | 'b' -> 2 | _ -> 0
EOF
      ;;
    658) cat > 0${i}_match_unit.ml << 'EOF'
let x = match u with | () -> 42
EOF
      ;;
    659) cat > 0${i}_match_poly_simple.ml << 'EOF'
let x = match v with | `A -> 1 | `B -> 2 | _ -> 0
EOF
      ;;
    660) cat > 0${i}_match_poly_arg.ml << 'EOF'
let x = match v with | `Point (x, y) -> x + y | _ -> 0
EOF
      ;;
  esac
done

# More type declarations (661-690)
for i in {661..690}; do
  case $i in
    661) cat > 0${i}_type_bool.ml << 'EOF'
type t = bool
EOF
      ;;
    662) cat > 0${i}_type_string.ml << 'EOF'
type t = string
EOF
      ;;
    663) cat > 0${i}_type_float.ml << 'EOF'
type t = float
EOF
      ;;
    664) cat > 0${i}_type_unit.ml << 'EOF'
type t = unit
EOF
      ;;
    665) cat > 0${i}_type_char.ml << 'EOF'
type t = char
EOF
      ;;
    666) cat > 0${i}_type_array.ml << 'EOF'
type t = int array
EOF
      ;;
    667) cat > 0${i}_type_ref.ml << 'EOF'
type t = int ref
EOF
      ;;
    668) cat > 0${i}_type_option_int.ml << 'EOF'
type t = int option
EOF
      ;;
    669) cat > 0${i}_type_list_int.ml << 'EOF'
type t = int list
EOF
      ;;
    670) cat > 0${i}_type_tuple_pair.ml << 'EOF'
type t = int * string
EOF
      ;;
    671) cat > 0${i}_type_arrow_simple.ml << 'EOF'
type t = int -> int
EOF
      ;;
    672) cat > 0${i}_type_variant_simple.ml << 'EOF'
type t = A | B
EOF
      ;;
    673) cat > 0${i}_type_variant_args.ml << 'EOF'
type t = A of int | B of string
EOF
      ;;
    674) cat > 0${i}_type_record_simple.ml << 'EOF'
type t = { x: int }
EOF
      ;;
    675) cat > 0${i}_type_record_two.ml << 'EOF'
type t = { x: int; y: int }
EOF
      ;;
    676) cat > 0${i}_type_poly_single.ml << 'EOF'
type 'a t = 'a
EOF
      ;;
    677) cat > 0${i}_type_poly_pair.ml << 'EOF'
type ('a, 'b) t = 'a * 'b
EOF
      ;;
    678) cat > 0${i}_type_poly_triple.ml << 'EOF'
type ('a, 'b, 'c) t = 'a * 'b * 'c
EOF
      ;;
    679) cat > 0${i}_type_rec_list.ml << 'EOF'
type 'a mylist = Nil | Cons of 'a * 'a mylist
EOF
      ;;
    680) cat > 0${i}_type_rec_tree.ml << 'EOF'
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
EOF
      ;;
    681) cat > 0${i}_type_nested_list.ml << 'EOF'
type t = int list list
EOF
      ;;
    682) cat > 0${i}_type_nested_option.ml << 'EOF'
type t = int option option
EOF
      ;;
    683) cat > 0${i}_type_arrow_tuple.ml << 'EOF'
type t = int * int -> int
EOF
      ;;
    684) cat > 0${i}_type_tuple_arrow.ml << 'EOF'
type t = (int -> int) * (int -> int)
EOF
      ;;
    685) cat > 0${i}_type_list_arrow.ml << 'EOF'
type t = (int -> int) list
EOF
      ;;
    686) cat > 0${i}_type_option_arrow.ml << 'EOF'
type t = (int -> int) option
EOF
      ;;
    687) cat > 0${i}_type_variant_tuple.ml << 'EOF'
type t = Point of int * int | Line of int * int * int * int
EOF
      ;;
    688) cat > 0${i}_type_variant_record.ml << 'EOF'
type t = Person of { name: string; age: int }
EOF
      ;;
    689) cat > 0${i}_type_record_nested.ml << 'EOF'
type t = { point: { x: int; y: int } }
EOF
      ;;
    690) cat > 0${i}_type_record_list.ml << 'EOF'
type t = { items: int list }
EOF
      ;;
  esac
done

# More let expressions (691-700)
for i in {691..700}; do
  case $i in
    691) cat > 0${i}_let_simple.ml << 'EOF'
let x = 42
EOF
      ;;
    692) cat > 0${i}_let_string.ml << 'EOF'
let s = "hello"
EOF
      ;;
    693) cat > 0${i}_let_bool.ml << 'EOF'
let b = true
EOF
      ;;
    694) cat > 0${i}_let_unit.ml << 'EOF'
let u = ()
EOF
      ;;
    695) cat > 0${i}_let_tuple.ml << 'EOF'
let t = (1, 2, 3)
EOF
      ;;
    696) cat > 0${i}_let_list.ml << 'EOF'
let l = [1; 2; 3; 4; 5]
EOF
      ;;
    697) cat > 0${i}_let_array.ml << 'EOF'
let a = [| 1; 2; 3 |]
EOF
      ;;
    698) cat > 0${i}_let_record.ml << 'EOF'
let r = { x = 1; y = 2 }
EOF
      ;;
    699) cat > 0${i}_let_fun.ml << 'EOF'
let f = fun x -> x + 1
EOF
      ;;
    700) cat > 0${i}_let_in_simple.ml << 'EOF'
let x = 1 in x + 1
EOF
      ;;
  esac
done

echo "Created 100 more test files (601-700)"
