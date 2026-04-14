open Std

type t = string

let from_string = fun name ->
  let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  let is_lowercase c = c >= 'a' && c <= 'z' in
  let is_digit c = c >= '0' && c <= '9' in
  let is_alphanum c = is_alpha c || is_digit c in
  let is_valid_char c = is_alphanum c || c = '-' || c = '_' in
  if String.length name = 0 then
    Error "Package name cannot be empty"
  else
    let first_char = String.get_unchecked name ~at:0 in
    let last_char = String.get_unchecked name ~at:(String.length name - 1) in
    if not (is_lowercase first_char && is_alpha first_char) then
      Error ("Package name must start with a lowercase letter. Try '"
      ^ String.lowercase_ascii name
      ^ "' instead")
    else if first_char = '-' || first_char = '_' then
      Error "Package name cannot start with hyphen or underscore"
    else if last_char = '-' || last_char = '_' then
      Error "Package name cannot end with hyphen or underscore"
    else if not (String.for_all ~fn:is_valid_char name) then
      Error "Package name can only contain lowercase letters, numbers, hyphens, and underscores"
    else
      Ok name

let to_string name = name

let equal = String.equal

let compare = String.compare
