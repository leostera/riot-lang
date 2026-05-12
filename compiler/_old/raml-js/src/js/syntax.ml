open Std

type property_name_kind =
  | Identifier
  | Quoted_string

let reserved_binding_identifiers = [
  "await";
  "break";
  "case";
  "catch";
  "class";
  "const";
  "continue";
  "debugger";
  "default";
  "delete";
  "do";
  "else";
  "export";
  "extends";
  "false";
  "finally";
  "for";
  "function";
  "if";
  "import";
  "in";
  "instanceof";
  "new";
  "null";
  "return";
  "super";
  "switch";
  "this";
  "throw";
  "true";
  "try";
  "typeof";
  "var";
  "void";
  "while";
  "with";
  "yield";
]

let is_reserved_binding_identifier = fun name ->
  List.exists (String.equal name) reserved_binding_identifiers

let is_ascii_uppercase = fun char ->
  if char >= 'A' then
    char <= 'Z'
  else
    false

let is_ascii_lowercase = fun char ->
  if char >= 'a' then
    char <= 'z'
  else
    false

let is_ascii_letter = fun char ->
  if is_ascii_lowercase char then
    true
  else
    is_ascii_uppercase char

let is_identifier_start = fun char ->
  if is_ascii_letter char then
    true
  else if char = '_' then
    true
  else
    char = '$'

let is_identifier_continue = fun char ->
  if is_identifier_start char then
    true
  else if char >= '0' then
    char <= '9'
  else
    false

let is_valid_identifier = fun name ->
  let length = String.length name in
  if length = 0 then
    false
  else if not (is_identifier_start (String.get_unchecked name ~at:0)) then
    false
  else
    let rec loop index =
      if index >= length then
        true
      else if is_identifier_continue (String.get_unchecked name ~at:index) then
        loop (index + 1)
      else
        false
    in
    loop 1

let is_valid_binding_identifier = fun name ->
  if is_valid_identifier name then
    not (is_reserved_binding_identifier name)
  else
    false

let sanitize_binding_identifier = fun name ->
  let length = String.length name in
  let buffer = IO.Buffer.create ~size:(max 1 length) in
  let push_valid_start char =
    if is_identifier_start char then
      IO.Buffer.add_char buffer char
    else if char >= '0' then
      if char <= '9' then
        begin
          IO.Buffer.add_char buffer '_';
          IO.Buffer.add_char buffer char
        end
      else if char = '\'' then
        begin
          IO.Buffer.add_char buffer '_';
          IO.Buffer.add_char buffer '$'
        end
      else
        IO.Buffer.add_char buffer '_'
    else if char = '\'' then
      begin
        IO.Buffer.add_char buffer '_';
        IO.Buffer.add_char buffer '$'
      end
    else
      IO.Buffer.add_char buffer '_'
  in
  let push_valid_continue char =
    if is_identifier_continue char then
      IO.Buffer.add_char buffer char
    else if char = '\'' then
      IO.Buffer.add_char buffer '$'
    else
      IO.Buffer.add_char buffer '_'
  in
  if length = 0 then
    "_"
  else begin
    push_valid_start (String.get_unchecked name ~at:0);
    let rec loop index =
      if index < length then
        begin
          push_valid_continue (String.get_unchecked name ~at:index);
          loop (index + 1)
        end
    in
    loop 1;
    let lowered = IO.Buffer.contents buffer in
    if is_reserved_binding_identifier lowered then
      "_" ^ lowered
    else
      lowered
  end

let classify_property_name = fun name ->
  if is_valid_identifier name then
    Identifier
  else
    Quoted_string

let can_use_dot_property = fun name -> classify_property_name name = Identifier

let can_use_unquoted_object_key = can_use_dot_property
