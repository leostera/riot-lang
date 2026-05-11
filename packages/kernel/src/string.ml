open Prelude

type t = string

type utf_decode = Unicode.Rune.utf_decode

let empty = ""

let length = Caml_runtime.string_length

let is_empty = fun value -> length value = 0

let unsafe_get = Caml_runtime.string_get

let get = fun value ~at ->
  if at < 0 || at >= length value then
    None
  else
    Some (unsafe_get value at)

let get_unchecked = fun value ~at -> unsafe_get value at

let sub = fun value ~offset ~len ->
  let value_length = length value in
  if offset < 0 || len < 0 || offset > value_length - len then
    System_error.panic "String.sub received an invalid slice";
  if len = 0 then
    empty
  else
    let out = Caml_runtime.bytes_create len in
    Caml_runtime.string_blit value offset out 0 len;
  Caml_runtime.bytes_unsafe_to_string out

let init = fun ~len ~fn ->
  let out = Caml_runtime.bytes_create len in
  let rec fill index =
    if index >= len then
      out
    else (
      Caml_runtime.bytes_set out index (fn index);
      fill (index + 1)
    )
  in
  let _ = fill 0 in
  Caml_runtime.bytes_unsafe_to_string out

let make = fun ~len ~char -> init ~len ~fn:(fun _ -> char)

let append = fun left right ->
  let left_length = length left in
  let right_length = length right in
  let out = Caml_runtime.bytes_create (left_length + right_length) in
  Caml_runtime.string_blit left 0 out 0 left_length;
  Caml_runtime.string_blit right 0 out left_length right_length;
  Caml_runtime.bytes_unsafe_to_string out

let concat = fun separator values ->
  let rec total_length acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> acc
    | [ value ] -> acc + length value
    | value :: rest -> total_length (acc + length value + length separator) rest
  in
  let rec fill out offset = fun __tmp1 ->
    match __tmp1 with
    | [] -> out
    | [ value ] ->
        let value_length = length value in
        Caml_runtime.string_blit value 0 out offset value_length;
        out
    | value :: rest ->
        let value_length = length value in
        let separator_length = length separator in
        Caml_runtime.string_blit value 0 out offset value_length;
        Caml_runtime.string_blit separator 0 out (offset + value_length) separator_length;
        fill out (offset + value_length + separator_length) rest
  in
  match values with
  | [] -> empty
  | [ value ] -> value
  | values ->
      let out = Caml_runtime.bytes_create (total_length 0 values) in
      Caml_runtime.bytes_unsafe_to_string (fill out 0 values)

let contains = fun haystack needle ->
  let haystack_length = length haystack in
  let needle_length = length needle in
  if needle_length = 0 then
    true
  else if needle_length > haystack_length then
    false
  else
    let rec matches offset index =
      if index >= needle_length then
        true
      else if unsafe_get haystack (offset + index) != unsafe_get needle index then
        false
      else
        matches offset (index + 1)
    in
    let rec search offset =
      if offset > haystack_length - needle_length then
        false
      else if matches offset 0 then
        true
      else
        search (offset + 1)
    in
    search 0

let starts_with = fun ~prefix value ->
  let prefix_length = length prefix in
  let value_length = length value in
  if prefix_length > value_length then
    false
  else
    let rec loop index =
      if index >= prefix_length then
        true
      else if unsafe_get prefix index != unsafe_get value index then
        false
      else
        loop (index + 1)
    in
    loop 0

let ends_with = fun ~suffix value ->
  let suffix_length = length suffix in
  let value_length = length value in
  if suffix_length > value_length then
    false
  else
    let offset = value_length - suffix_length in
    let rec loop index =
      if index >= suffix_length then
        true
      else if unsafe_get suffix index != unsafe_get value (offset + index) then
        false
      else
        loop (index + 1)
    in
    loop 0

let equal = Caml_runtime.equal

let compare = Order.compare

let index_of = fun value ~char:needle ->
  let rec loop index =
    if index >= length value then
      None
    else if unsafe_get value index = needle then
      Some index
    else
      loop (index + 1)
  in
  loop 0

let last_index = fun value needle ->
  let rec loop index =
    if index < 0 then
      None
    else if unsafe_get value index = needle then
      Some index
    else
      loop (index - 1)
  in
  loop (length value - 1)

let is_trim_char = fun __tmp1 ->
  match __tmp1 with
  | ' '
  | '\t'
  | '\n'
  | '\r'
  | '\011'
  | '\012' -> true
  | _ -> false

let trim = fun value ->
  let value_length = length value in
  let rec find_start index =
    if index >= value_length then
      value_length
    else if is_trim_char (unsafe_get value index) then
      find_start (index + 1)
    else
      index
  in
  let rec find_end index =
    if index < 0 then (
      (-1)
    ) else if is_trim_char (unsafe_get value index) then
      find_end (index - 1)
    else
      index
  in
  let start = find_start 0 in
  let finish = find_end (value_length - 1) in
  if finish < start then
    empty
  else
    sub value ~offset:start ~len:(finish - start + 1)

let split = fun ~by value ->
  let separator_length = length by in
  let value_length = length value in
  if separator_length = 0 then
    [ value ]
  else
    let rec matches offset index =
      if index >= separator_length then
        true
      else if unsafe_get value (offset + index) != unsafe_get by index then
        false
      else
        matches offset (index + 1)
    in
    let rec loop start index acc =
      if index > value_length - separator_length then
        List.reverse (sub value ~offset:start ~len:(value_length - start) :: acc)
      else if matches index 0 then
        loop
          (index + separator_length)
          (index + separator_length)
          (sub value ~offset:start ~len:(index - start) :: acc)
      else
        loop start (index + 1) acc
    in
    loop 0 0 []

let split_on_char = fun separator value ->
  let value_length = length value in
  let rec loop start index acc =
    if index >= value_length then
      List.reverse (sub value ~offset:start ~len:(value_length - start) :: acc)
    else if unsafe_get value index = separator then
      loop (index + 1) (index + 1) (sub value ~offset:start ~len:(index - start) :: acc)
    else
      loop start (index + 1) acc
  in
  loop 0 0 []

let lowercase_ascii_char = fun value ->
  let code = Char.to_int value in
  if code >= Char.to_int 'A' && code <= Char.to_int 'Z' then
    Char.from_int_unchecked (code + 32)
  else
    value

let uppercase_ascii_char = fun value ->
  let code = Char.to_int value in
  if code >= Char.to_int 'a' && code <= Char.to_int 'z' then
    Char.from_int_unchecked (code - 32)
  else
    value

let lowercase_ascii = fun value ->
  init
    ~len:(length value)
    ~fn:(fun index -> lowercase_ascii_char (unsafe_get value index))

let capitalize_ascii = fun value ->
  if is_empty value then
    empty
  else
    let first = unsafe_get value 0 in
    let code = Char.to_int first in
    let first =
      if code >= Char.to_int 'a' && code <= Char.to_int 'z' then
        Char.from_int_unchecked (code - 32)
      else
        first
    in
    init
      ~len:(length value)
      ~fn:(fun index ->
        if index = 0 then
          first
        else
          unsafe_get value index)

let uppercase_ascii = fun value ->
  init
    ~len:(length value)
    ~fn:(fun index -> uppercase_ascii_char (unsafe_get value index))

let map = fun ~fn value -> init ~len:(length value) ~fn:(fun index -> fn (unsafe_get value index))

let for_each = fun ~fn value ->
  let rec loop index =
    if index >= length value then
      ()
    else (
      fn (unsafe_get value index);
      loop (index + 1)
    )
  in
  loop 0

let exists = fun ~fn:predicate value ->
  let rec loop index =
    if index >= length value then
      false
    else if predicate (unsafe_get value index) then
      true
    else
      loop (index + 1)
  in
  loop 0

let for_all = fun ~fn value ->
  let rec loop index =
    if index >= length value then
      true
    else if fn (unsafe_get value index) then
      loop (index + 1)
    else
      false
  in
  loop 0

let fold_left = fun ~fn ~acc value ->
  let rec loop index acc =
    if index >= length value then
      acc
    else
      loop (index + 1) (fn acc (unsafe_get value index))
  in
  loop 0 acc

let escaped =
  let decimal_digit value = Char.from_int_unchecked (Char.to_int '0' + value) in
  let escaped_length value =
    fold_left
      ~fn:(fun acc char ->
        match char with
        | '"'
        | '\\'
        | '\n'
        | '\t'
        | '\r'
        | '\008'
        | '\012' -> acc + 2
        | _ ->
            let code = Char.to_int char in
            if code < 32 || code > 126 then
              acc + 4
            else
              acc + 1)
      ~acc:0
      value
  in
  fun value ->
    let out = Caml_runtime.bytes_create (escaped_length value) in
    let push_decimal offset code =
      Caml_runtime.bytes_set out offset '\\';
      Caml_runtime.bytes_set out (offset + 1) (decimal_digit ((code / 100) mod 10));
      Caml_runtime.bytes_set out (offset + 2) (decimal_digit ((code / 10) mod 10));
      Caml_runtime.bytes_set out (offset + 3) (decimal_digit (code mod 10));
      offset + 4
    in
    let push_escape offset escaped =
      Caml_runtime.bytes_set out offset '\\';
      Caml_runtime.bytes_set out (offset + 1) escaped;
      offset + 2
    in
    let rec fill index offset =
      if index >= length value then
        Caml_runtime.bytes_unsafe_to_string out
      else
        let char = unsafe_get value index in
        let next_offset =
          match char with
          | '"' -> push_escape offset '"'
          | '\\' -> push_escape offset '\\'
          | '\n' -> push_escape offset 'n'
          | '\t' -> push_escape offset 't'
          | '\r' -> push_escape offset 'r'
          | '\008' -> push_escape offset 'b'
          | '\012' -> push_escape offset 'f'
          | _ ->
              let code = Char.to_int char in
              if code < 32 || code > 126 then
                push_decimal offset code
              else (
                Caml_runtime.bytes_set out offset char;
                offset + 1
              )
        in
        fill (index + 1) next_offset
    in
    fill 0 0

let get_utf_8_rune =
  let not_in_x80_to_xBF value = value lsr 6 != 0b10 in
  let not_in_xA0_to_xBF value = value lsr 5 != 0b101 in
  let not_in_x80_to_x9F value = value lsr 5 != 0b100 in
  let not_in_x90_to_xBF value = value < 0x90 || value > 0xbf in
  let not_in_x80_to_x8F value = value lsr 4 != 0x8 in
  let utf_8_rune_2 b0 b1 = ((b0 land 0x1f) lsl 6) lor (b1 land 0x3f) in
  let utf_8_rune_3 b0 b1 b2 = ((b0 land 0x0f) lsl 12) lor ((b1 land 0x3f) lsl 6) lor (b2 land 0x3f)
  in
  let utf_8_rune_4 b0 b1 b2 b3 =
    ((b0 land 0x07) lsl 18) lor ((b1 land 0x3f) lsl 12) lor ((b2 land 0x3f) lsl 6) lor (b3 land 0x3f)
  in
  fun source ~at:index ->
    if index < 0 || index >= length source then
      None
    else
      let b0 =
        unsafe_get source index
        |> Char.to_int
      in
      let max_index = length source - 1 in
      let get_byte byte_index =
        unsafe_get source byte_index
        |> Char.to_int
      in
      Some (
        match Char.from_int_unchecked b0 with
        | '\x00' .. '\x7F' -> Unicode.Rune.utf_decode 1 (Unicode.Rune.from_int_unchecked b0)
        | '\xC2' .. '\xDF' ->
            let index = index + 1 in
            if index > max_index then
              Unicode.Rune.utf_decode_invalid 1
            else
              let b1 = get_byte index in
              if not_in_x80_to_xBF b1 then
                Unicode.Rune.utf_decode_invalid 1
              else
                Unicode.Rune.utf_decode 2 (Unicode.Rune.from_int_unchecked (utf_8_rune_2 b0 b1))
        | '\xE0' ->
            let index = index + 1 in
            if index > max_index then
              Unicode.Rune.utf_decode_invalid 1
            else
              let b1 = get_byte index in
              if not_in_xA0_to_xBF b1 then
                Unicode.Rune.utf_decode_invalid 1
              else
                let index = index + 1 in
                if index > max_index then
                  Unicode.Rune.utf_decode_invalid 2
                else
                  let b2 = get_byte index in
                  if not_in_x80_to_xBF b2 then
                    Unicode.Rune.utf_decode_invalid 2
                  else
                    Unicode.Rune.utf_decode
                      3
                      (Unicode.Rune.from_int_unchecked (utf_8_rune_3 b0 b1 b2))
        | '\xE1' .. '\xEC'
        | '\xEE' .. '\xEF' ->
            let index = index + 1 in
            if index > max_index then
              Unicode.Rune.utf_decode_invalid 1
            else
              let b1 = get_byte index in
              if not_in_x80_to_xBF b1 then
                Unicode.Rune.utf_decode_invalid 1
              else
                let index = index + 1 in
                if index > max_index then
                  Unicode.Rune.utf_decode_invalid 2
                else
                  let b2 = get_byte index in
                  if not_in_x80_to_xBF b2 then
                    Unicode.Rune.utf_decode_invalid 2
                  else
                    Unicode.Rune.utf_decode
                      3
                      (Unicode.Rune.from_int_unchecked (utf_8_rune_3 b0 b1 b2))
        | '\xED' ->
            let index = index + 1 in
            if index > max_index then
              Unicode.Rune.utf_decode_invalid 1
            else
              let b1 = get_byte index in
              if not_in_x80_to_x9F b1 then
                Unicode.Rune.utf_decode_invalid 1
              else
                let index = index + 1 in
                if index > max_index then
                  Unicode.Rune.utf_decode_invalid 2
                else
                  let b2 = get_byte index in
                  if not_in_x80_to_xBF b2 then
                    Unicode.Rune.utf_decode_invalid 2
                  else
                    Unicode.Rune.utf_decode
                      3
                      (Unicode.Rune.from_int_unchecked (utf_8_rune_3 b0 b1 b2))
        | '\xF0' ->
            let index = index + 1 in
            if index > max_index then
              Unicode.Rune.utf_decode_invalid 1
            else
              let b1 = get_byte index in
              if not_in_x90_to_xBF b1 then
                Unicode.Rune.utf_decode_invalid 1
              else
                let index = index + 1 in
                if index > max_index then
                  Unicode.Rune.utf_decode_invalid 2
                else
                  let b2 = get_byte index in
                  if not_in_x80_to_xBF b2 then
                    Unicode.Rune.utf_decode_invalid 2
                  else
                    let index = index + 1 in
                    if index > max_index then
                      Unicode.Rune.utf_decode_invalid 3
                    else
                      let b3 = get_byte index in
                      if not_in_x80_to_xBF b3 then
                        Unicode.Rune.utf_decode_invalid 3
                      else
                        Unicode.Rune.utf_decode
                          4
                          (Unicode.Rune.from_int_unchecked (utf_8_rune_4 b0 b1 b2 b3))
        | '\xF1' .. '\xF3' ->
            let index = index + 1 in
            if index > max_index then
              Unicode.Rune.utf_decode_invalid 1
            else
              let b1 = get_byte index in
              if not_in_x80_to_xBF b1 then
                Unicode.Rune.utf_decode_invalid 1
              else
                let index = index + 1 in
                if index > max_index then
                  Unicode.Rune.utf_decode_invalid 2
                else
                  let b2 = get_byte index in
                  if not_in_x80_to_xBF b2 then
                    Unicode.Rune.utf_decode_invalid 2
                  else
                    let index = index + 1 in
                    if index > max_index then
                      Unicode.Rune.utf_decode_invalid 3
                    else
                      let b3 = get_byte index in
                      if not_in_x80_to_xBF b3 then
                        Unicode.Rune.utf_decode_invalid 3
                      else
                        Unicode.Rune.utf_decode
                          4
                          (Unicode.Rune.from_int_unchecked (utf_8_rune_4 b0 b1 b2 b3))
        | '\xF4' ->
            let index = index + 1 in
            if index > max_index then
              Unicode.Rune.utf_decode_invalid 1
            else
              let b1 = get_byte index in
              if not_in_x80_to_x8F b1 then
                Unicode.Rune.utf_decode_invalid 1
              else
                let index = index + 1 in
                if index > max_index then
                  Unicode.Rune.utf_decode_invalid 2
                else
                  let b2 = get_byte index in
                  if not_in_x80_to_xBF b2 then
                    Unicode.Rune.utf_decode_invalid 2
                  else
                    let index = index + 1 in
                    if index > max_index then
                      Unicode.Rune.utf_decode_invalid 3
                    else
                      let b3 = get_byte index in
                      if not_in_x80_to_xBF b3 then
                        Unicode.Rune.utf_decode_invalid 3
                      else
                        Unicode.Rune.utf_decode
                          4
                          (Unicode.Rune.from_int_unchecked (utf_8_rune_4 b0 b1 b2 b3))
        | _ -> Unicode.Rune.utf_decode_invalid 1
      )

let from_bytes = Caml_runtime.bytes_to_string

let unsafe_from_bytes = Caml_runtime.bytes_unsafe_to_string

let to_bytes = Caml_runtime.bytes_of_string
