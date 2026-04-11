open Std

type property_name_kind =
  | Identifier
  | Quoted_string

let is_ascii_uppercase = fun char -> char >= 'A' && char <= 'Z'

let is_ascii_lowercase = fun char -> char >= 'a' && char <= 'z'

let is_ascii_letter = fun char ->
  is_ascii_lowercase char || is_ascii_uppercase char

let is_identifier_start = fun char ->
  is_ascii_letter char || char = '_' || char = '$'

let is_identifier_continue = fun char ->
  is_identifier_start char || (char >= '0' && char <= '9')

let is_valid_identifier = fun name ->
  let length = String.length name in
  if length = 0 then
    false
  else if not (is_identifier_start name.[0]) then
    false
  else
    let rec loop index =
      if index >= length then
        true
      else if is_identifier_continue name.[index] then
        loop (index + 1)
      else
        false
    in
    loop 1

let classify_property_name = fun name ->
  if is_valid_identifier name then
    Identifier
  else
    Quoted_string

let can_use_dot_property = fun name ->
  classify_property_name name = Identifier

let can_use_unquoted_object_key = can_use_dot_property
