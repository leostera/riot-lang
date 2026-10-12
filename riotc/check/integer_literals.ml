// Decimal integer literal range checks for the fixed-width RiotML integer types.
use Prelude.{Option, Some, None}
use String.{length, get, slice}

let first_nonzero_index = fn digits -> fn offset -> match (offset >= length digits) {
  | true -> length digits
  | false -> match (get digits offset) {
    | Some(48) -> first_nonzero_index digits (offset + 1)
    | Some(_) -> offset
    | None -> length digits
  }
}

let normalized_magnitude = fn digits -> {
  let offset = first_nonzero_index digits 0;
  match (offset >= length digits) {
    | true -> "0"
    | false -> slice digits offset (length digits)
  }
}

let compare_equal_length_digits = fn first -> fn second -> fn offset ->
  match (offset >= length first) {
    | true -> 0
    | false -> match (get first offset) {
      | None -> 0
      | Some(first_digit) -> match (get second offset) {
        | None -> 0
        | Some(second_digit) -> match (first_digit < second_digit) {
          | true -> -1
          | false -> match (first_digit > second_digit) {
            | true -> 1
            | false -> compare_equal_length_digits first second (offset + 1)
          }
        }
      }
    }
  }

let compare_magnitudes = fn first -> fn second -> match (length first < length second) {
  | true -> -1
  | false -> match (length first > length second) {
    | true -> 1
    | false -> compare_equal_length_digits first second 0
  }
}

let literal_parts = fn literal -> match (get literal 0) {
  | Some(45) -> (true, normalized_magnitude (slice literal 1 (length literal)))
  | _ -> (false, normalized_magnitude literal)
}

let fits_signed = fn literal -> fn positive_limit -> fn negative_limit -> {
  let (is_negative, magnitude) = literal_parts literal;
  let limit = match is_negative {
    | true -> negative_limit
    | false -> positive_limit
  };
  compare_magnitudes magnitude limit <= 0
}

let fits_unsigned = fn literal -> fn limit -> {
  let (is_negative, magnitude) = literal_parts literal;
  match is_negative {
    | true -> false
    | false -> compare_magnitudes magnitude limit <= 0
  }
}

let integer_type_name = fn name -> match name {
  | "i8" -> true
  | "i16" -> true
  | "i32" -> true
  | "i64" -> true
  | "u8" -> true
  | "u16" -> true
  | "u32" -> true
  | "u64" -> true
  | _ -> false
}

let integer_literal_fits = fn literal -> fn name -> match name {
  | "i8" -> fits_signed literal "127" "128"
  | "i16" -> fits_signed literal "32767" "32768"
  | "i32" -> fits_signed literal "2147483647" "2147483648"
  | "i64" -> fits_signed literal "9223372036854775807" "9223372036854775808"
  | "u8" -> fits_unsigned literal "255"
  | "u16" -> fits_unsigned literal "65535"
  | "u32" -> fits_unsigned literal "4294967295"
  | "u64" -> fits_unsigned literal "18446744073709551615"
  | _ -> false
}
