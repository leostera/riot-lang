use Prelude.{List, Nil, Cons}
use Check.IntegerLiterals.{integer_type_name, integer_literal_fits}

let append = fn first -> fn second -> match first {
  | Nil -> second
  | Cons(value, remaining) -> Cons(value, append remaining second)
}

let all_true = fn values -> match values {
  | Nil -> true
  | Cons(true, remaining) -> all_true remaining
  | Cons(false, _) -> false
}

let does_not_fit = fn literal -> fn type_name ->
  match (integer_literal_fits literal type_name) {
    | true -> false
    | false -> true
  }

let signed_i8_checks = Cons(integer_literal_fits "127" "i8",
  Cons(integer_literal_fits "-128" "i8",
  Cons(does_not_fit "128" "i8", Cons(does_not_fit "-129" "i8", Nil))))
let signed_i16_checks = Cons(integer_literal_fits "32767" "i16",
  Cons(integer_literal_fits "-32768" "i16",
  Cons(does_not_fit "32768" "i16", Cons(does_not_fit "-32769" "i16", Nil))))
let signed_i32_checks = Cons(integer_literal_fits "2147483647" "i32",
  Cons(integer_literal_fits "-2147483648" "i32",
  Cons(does_not_fit "2147483648" "i32", Cons(does_not_fit "-2147483649" "i32", Nil))))
let signed_i64_checks = Cons(integer_literal_fits "9223372036854775807" "i64",
  Cons(integer_literal_fits "-9223372036854775808" "i64",
  Cons(does_not_fit "9223372036854775808" "i64",
    Cons(does_not_fit "-9223372036854775809" "i64", Nil))))
let unsigned_checks = Cons(integer_literal_fits "255" "u8",
  Cons(does_not_fit "256" "u8",
  Cons(does_not_fit "-1" "u8",
  Cons(integer_literal_fits "65535" "u16",
  Cons(does_not_fit "65536" "u16",
  Cons(integer_literal_fits "4294967295" "u32",
  Cons(does_not_fit "4294967296" "u32",
  Cons(integer_literal_fits "18446744073709551615" "u64",
  Cons(does_not_fit "18446744073709551616" "u64", Nil)))))))))
let type_names = Cons(integer_type_name "i8",
  Cons(integer_type_name "i16",
  Cons(integer_type_name "i32",
  Cons(integer_type_name "i64",
  Cons(integer_type_name "u8",
  Cons(integer_type_name "u16",
  Cons(integer_type_name "u32", Cons(integer_type_name "u64", Nil))))))))

let range_checks = append type_names (append signed_i8_checks
  (append signed_i16_checks (append signed_i32_checks
    (append signed_i64_checks (append unsigned_checks (Cons(
      integer_literal_fits "0000000000000000007" "u64",
      Cons(does_not_fit "1" "unknown", Nil))))))))

let main = fn () -> match (all_true range_checks) {
  | true -> 0
  | false -> 1
}
