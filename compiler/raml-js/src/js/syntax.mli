type property_name_kind =
  | Identifier
  | Quoted_string
val is_reserved_binding_identifier: string -> bool

val is_ascii_uppercase: char -> bool

val is_ascii_lowercase: char -> bool

val is_ascii_letter: char -> bool

val is_identifier_start: char -> bool

val is_identifier_continue: char -> bool

val is_valid_identifier: string -> bool

val is_valid_binding_identifier: string -> bool

val sanitize_binding_identifier: string -> string

val classify_property_name: string -> property_name_kind

val can_use_dot_property: string -> bool

val can_use_unquoted_object_key: string -> bool
