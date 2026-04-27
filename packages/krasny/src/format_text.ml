open Std
open Std.Collections

module Slice = IO.IoVec.IoSlice

let last_line_width = fun text ->
  let length = String.length text in
  let rec loop index =
    if Int.(index < 0) then
      length
    else if Char.equal (String.get_unchecked text ~at:index) '\n' then
      Int.sub (Int.sub length index) 1
    else
      loop (Int.sub index 1)
  in
  loop (Int.sub length 1)

let last_slice_line_width = fun slice ->
  let rec find_last_newline index last_newline =
    if Int.(index >= Slice.length slice) then
      last_newline
    else if Char.equal (Slice.get_unchecked slice ~at:index) '\n' then
      find_last_newline Int.(index + 1) index
    else
      find_last_newline Int.(index + 1) last_newline
  in
  let last_newline = find_last_newline 0 (-1) in
  if Int.(last_newline < 0) then
    Slice.length slice
  else
    Int.(Slice.length slice - last_newline - 1)

let count_slice_newlines = fun slice ->
  let length = Slice.length slice in
  let rec loop index count =
    if Int.(index >= length) then
      count
    else if Char.equal (Slice.get_unchecked slice ~at:index) '\n' then
      loop (Int.add index 1) (Int.add count 1)
    else
      loop (Int.add index 1) count
  in
  loop 0 0

let is_horizontal_whitespace = fun char -> Char.equal char ' ' || Char.equal char '\t'

let trim_whitespace_only_segment = fun value ~start ~stop ->
  let rec loop index =
    if Int.(index >= stop) then
      start
    else if is_horizontal_whitespace (String.get_unchecked value ~at:index) then
      loop (Int.add index 1)
    else
      stop
  in
  loop start

let count_newlines = fun text ->
  let length = String.length text in
  let rec loop index count =
    if Int.(index >= length) then
      count
    else if Char.equal (String.get_unchecked text ~at:index) '\n' then
      loop (Int.add index 1) (Int.add count 1)
    else
      loop (Int.add index 1) count
  in
  loop 0 0

let split_lines = fun text ->
  let length = String.length text in
  let lines = Vector.with_capacity ~size:(Int.add (count_newlines text) 1) in
  let rec loop segment_start index =
    if Int.(index >= length) then
      Vector.push
        lines
        ~value:(String.sub
          text
          ~offset:segment_start
          ~len:(Int.sub length segment_start))
    else if Char.equal (String.get_unchecked text ~at:index) '\n' then (
      Vector.push
        lines
        ~value:(String.sub
          text
          ~offset:segment_start
          ~len:(Int.sub index segment_start));
      loop (Int.add index 1) (Int.add index 1)
    ) else
      loop segment_start (Int.add index 1)
  in
  loop 0 0;
  lines

let char_is_int_suffix = function
  | 'l'
  | 'L'
  | 'n' -> true
  | _ -> false

let remove_digit_separators = fun text ->
  if not (String.contains text "_") then
    text
  else
    let length = String.length text in
    let buffer = IO.Buffer.create ~size:length in
    for index = 0 to Int.sub length 1 do
      let char = String.get_unchecked text ~at:index in
      if not (Char.equal char '_') then
        IO.Buffer.add_char buffer char
    done;
  IO.Buffer.contents buffer

let add_grouped_from_right = fun buffer text ~group ->
  let length = String.length text in
  if Int.(length <= group) then
    IO.Buffer.add_string buffer text
  else
    let first_group =
      let remainder = Int.rem length group in
      if Int.equal remainder 0 then
        group
      else
        remainder
    in
    IO.Buffer.add_substring buffer text 0 first_group;
  let rec loop index =
    if Int.(index < length) then (
      IO.Buffer.add_char buffer '_';
      let remaining = Int.sub length index in
      let width = Int.min group remaining in
      IO.Buffer.add_substring buffer text index width;
      loop (Int.add index width)
    )
  in
  loop first_group

let add_grouped_from_left = fun buffer text ~group ->
  let length = String.length text in
  let rec loop index =
    if Int.(index < length) then (
      if Int.(index > 0) then
        IO.Buffer.add_char buffer '_';
      let remaining = Int.sub length index in
      let width = Int.min group remaining in
      IO.Buffer.add_substring buffer text index width;
      loop (Int.add index width)
    )
  in
  loop 0

let grouped_from_right = fun text ~group ->
  let text = remove_digit_separators text in
  let buffer =
    IO.Buffer.create ~size:(Int.add (String.length text) (Int.div (String.length text) group))
  in
  add_grouped_from_right buffer text ~group;
  IO.Buffer.contents buffer

let grouped_from_left = fun text ~group ->
  let text = remove_digit_separators text in
  let buffer =
    IO.Buffer.create ~size:(Int.add (String.length text) (Int.div (String.length text) group))
  in
  add_grouped_from_left buffer text ~group;
  IO.Buffer.contents buffer

let format_int_literal = fun text ->
  let length = String.length text in
  if Int.equal length 0 then
    text
  else
    let (core_length, suffix) =
      let last = String.get_unchecked text ~at:(Int.sub length 1) in
      if char_is_int_suffix last then
        (Int.sub length 1, String.make ~len:1 ~char:last)
      else
        (length, "")
    in
    let core = String.sub text ~offset:0 ~len:core_length in
    let core_length = String.length core in
    if Int.(core_length >= 2) then
      let prefix = String.sub core ~offset:0 ~len:2 in
      let digits = String.sub
        core
        ~offset:2
        ~len:(Int.sub core_length 2)
      in
      if String.equal prefix "0x" || String.equal prefix "0X" then
        "0x" ^ grouped_from_right (String.lowercase_ascii digits) ~group:4 ^ suffix
      else if String.equal prefix "0b" || String.equal prefix "0B" then
        "0b" ^ grouped_from_right digits ~group:4 ^ suffix
      else if String.equal prefix "0o" || String.equal prefix "0O" then
        "0o" ^ grouped_from_right digits ~group:3 ^ suffix
      else
        grouped_from_right core ~group:3 ^ suffix
    else
      grouped_from_right core ~group:3 ^ suffix

let first_exponent_index = fun text ->
  let length = String.length text in
  let rec loop index =
    if Int.(index >= length) then
      None
    else
      match String.get_unchecked text ~at:index with
      | 'e'
      | 'E' -> Some index
      | _ -> loop (Int.add index 1)
  in
  loop 0

let format_float_mantissa = fun mantissa ->
  match String.index_of mantissa ~char:'.' with
  | None -> grouped_from_right mantissa ~group:3
  | Some dot ->
      let left = String.sub mantissa ~offset:0 ~len:dot in
      let right =
        String.sub
          mantissa
          ~offset:(Int.add dot 1)
          ~len:(Int.sub (String.length mantissa) (Int.add dot 1))
      in
      grouped_from_right left ~group:3 ^ "." ^ grouped_from_left right ~group:3

let format_float_exponent = fun exponent ->
  let exponent = remove_digit_separators exponent in
  if String.is_empty exponent then
    exponent
  else
    let first = String.get_unchecked exponent ~at:0 in
    let first =
      match first with
      | 'E' -> 'e'
      | _ -> first
    in
    String.make ~len:1 ~char:first
    ^ String.sub
      exponent
      ~offset:1
      ~len:(Int.sub (String.length exponent) 1)

let format_float_literal = fun text ->
  match first_exponent_index text with
  | None -> format_float_mantissa text
  | Some exponent_index ->
      let mantissa = String.sub text ~offset:0 ~len:exponent_index in
      let exponent =
        String.sub
          text
          ~offset:exponent_index
          ~len:(Int.sub (String.length text) exponent_index)
      in
      format_float_mantissa mantissa ^ format_float_exponent exponent

let is_blank_line = fun line -> String.is_empty (String.trim line)

let leading_horizontal_width = fun line ->
  let length = String.length line in
  let rec loop index =
    if Int.(index >= length) then
      length
    else if is_horizontal_whitespace (String.get_unchecked line ~at:index) then
      loop (Int.add index 1)
    else
      index
  in
  loop 0

let trim_trailing_horizontal = fun line ->
  let length = String.length line in
  let rec loop index =
    if Int.(index < 0) then
      0
    else if is_horizontal_whitespace (String.get_unchecked line ~at:index) then
      loop (Int.sub index 1)
    else
      Int.add index 1
  in
  String.sub line ~offset:0 ~len:(loop (Int.sub length 1))

let strip_leading_width = fun line width ->
  let length = String.length line in
  let rec loop index remaining =
    if Int.(remaining <= 0 || index >= length) then
      index
    else if is_horizontal_whitespace (String.get_unchecked line ~at:index) then
      loop (Int.add index 1) (Int.sub remaining 1)
    else
      index
  in
  let offset = loop 0 width in
  String.sub
    line
    ~offset
    ~len:(Int.sub length offset)

let first_nonblank_line_index = fun lines ->
  let length = Vector.length lines in
  let rec loop index =
    if Int.(index >= length) then
      None
    else if is_blank_line (Vector.get_unchecked lines ~at:index) then
      loop (Int.add index 1)
    else
      Some index
  in
  loop 0

let last_nonblank_line_index = fun lines ->
  let rec loop index =
    if Int.(index < 0) then
      None
    else if is_blank_line (Vector.get_unchecked lines ~at:index) then
      loop (Int.sub index 1)
    else
      Some index
  in
  loop (Int.sub (Vector.length lines) 1)

let common_docstring_indent = fun lines ~start ~stop ->
  let rec loop index current =
    if Int.(index > stop) then
      current
    else
      let line = Vector.get_unchecked lines ~at:index in
      if is_blank_line line then
        loop (Int.add index 1) current
      else
        let indent = leading_horizontal_width line in
        let current =
          match current with
          | None -> Some indent
          | Some value -> Some (Int.min value indent)
        in
        loop (Int.add index 1) current
  in
  match loop start None with
  | Some indent -> indent
  | None -> 0
