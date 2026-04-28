open Std

type same_site =
  | Strict
  | Lax
  | None

type t = {
  name: string;
  value: string;
  max_age: int option;
  expires: string option;
  domain: string option;
  path: string;
  secure: bool;
  http_only: bool;
  same_site: same_site option;
}

type value_character_error =
  | ControlCharacter
  | Semicolon
  | Comma

type attribute =
  | Expires
  | Domain
  | Path

type attribute_character_error =
  | AttributeControlCharacter
  | AttributeSemicolon

type validation_error =
  | EmptyName
  | InvalidNameCharacter of { index: int; character: char }
  | InvalidValueCharacter of { index: int; character: char; reason: value_character_error }
  | InvalidAttributeCharacter of {
      attribute: attribute;
      index: int;
      character: char;
      reason: attribute_character_error;
    }
  | SameSiteNoneRequiresSecure
  | SecurePrefixRequiresSecure
  | HostPrefixRequiresSecure
  | HostPrefixRequiresNoDomain
  | HostPrefixRequiresRootPath

let character_code = fun character -> Int.to_string (Char.to_int character)

let value_character_error_to_string = function
  | ControlCharacter -> "control character"
  | Semicolon -> "semicolon"
  | Comma -> "comma"

let attribute_to_string = function
  | Expires -> "Expires"
  | Domain -> "Domain"
  | Path -> "Path"

let attribute_character_error_to_string = function
  | AttributeControlCharacter -> "control character"
  | AttributeSemicolon -> "semicolon"

let validation_error_to_string = function
  | EmptyName -> "Cookie name is empty"
  | InvalidNameCharacter { index; character } ->
      "Cookie name contains invalid character code "
      ^ character_code character
      ^ " at index "
      ^ Int.to_string index
  | InvalidValueCharacter { index; character; reason } ->
      "Cookie value contains "
      ^ value_character_error_to_string reason
      ^ " character code "
      ^ character_code character
      ^ " at index "
      ^ Int.to_string index
  | InvalidAttributeCharacter {
    attribute;
    index;
    character;
    reason
  } ->
      "Cookie "
      ^ attribute_to_string attribute
      ^ " contains "
      ^ attribute_character_error_to_string reason
      ^ " character code "
      ^ character_code character
      ^ " at index "
      ^ Int.to_string index
  | SameSiteNoneRequiresSecure -> "Cookie SameSite=None requires Secure"
  | SecurePrefixRequiresSecure -> "Cookie __Secure- prefix requires Secure"
  | HostPrefixRequiresSecure -> "Cookie __Host- prefix requires Secure"
  | HostPrefixRequiresNoDomain -> "Cookie __Host- prefix must not set Domain"
  | HostPrefixRequiresRootPath -> "Cookie __Host- prefix requires Path=/"

(** Parse Cookie header: "name1=value1; name2=value2" *)
let parse = fun header ->
  String.split ~by:";" header
  |> List.filter_map
    ~fn:(fun pair ->
      let trimmed = String.trim pair in
      match String.split ~by:"=" trimmed with
      | [] -> Option.none
      | [ name ] -> Some (String.trim name, "")
      | name :: value_parts ->
          let value = String.concat "=" value_parts in
          Some (String.trim name, String.trim value))

(** Helper: Parse Set-Cookie attribute *)
let parse_attribute = fun attr ->
  let trimmed = String.trim attr in
  match String.split ~by:"=" trimmed with
  | [] -> (Option.none, Option.none)
  | [ key ] -> (Some (String.lowercase_ascii (String.trim key)), Option.none)
  | key :: value_parts ->
      let value = String.concat "=" value_parts in
      (Some (String.lowercase_ascii (String.trim key)), Some (String.trim value))

(** Parse Set-Cookie header *)
let parse_set_cookie = fun header ->
  match String.split ~by:";" header with
  | [] -> Option.none
  | first :: attrs -> (
      match String.split ~by:"=" first with
      | name :: value_parts ->
          let cookie = {
            name = String.trim name;
            value =
              String.concat "=" value_parts
              |> String.trim;
            max_age = Option.none;
            expires = Option.none;
            domain = Option.none;
            path = "/";
            secure = false;
            http_only = false;
            same_site = Option.none;
          }
          in
          (* Parse attributes *)
          let cookie =
            List.fold_left
              attrs
              ~init:cookie
              ~fn:(fun c attr ->
                match parse_attribute attr with
                | (Some "max-age", Some value) -> (
                    match Int.parse value with
                    | Some age -> { c with max_age = Some age }
                    | Option.None -> c
                  )
                | (Some "expires", Some value) -> { c with expires = Some value }
                | (Some "path", Some value) -> { c with path = value }
                | (Some "domain", Some value) -> { c with domain = Some value }
                | (Some "secure", Option.None) -> { c with secure = true }
                | (Some "httponly", Option.None) -> { c with http_only = true }
                | (Some "samesite", Some value) ->
                    let same_site =
                      match String.lowercase_ascii value with
                      | "strict" -> Some Strict
                      | "lax" -> Some Lax
                      | "none" -> Some None
                      | _ -> Option.none
                    in
                    { c with same_site }
                | _ -> c)
          in
          Some cookie
      | [] -> Option.none
    )

(** Serialize SameSite to string *)
let same_site_to_string = function
  | Strict -> "Strict"
  | Lax -> "Lax"
  | None -> "None"

(** Serialize cookie to Set-Cookie header value *)
let to_set_cookie = fun t ->
  (* Start with name=value *)
  let parts = [ String.concat "=" [ t.name; t.value ] ] in
  (* Add Max-Age *)
  let parts =
    match t.max_age with
    | Some age -> (String.concat "=" [ "Max-Age"; Int.to_string age ]) :: parts
    | Option.None -> parts
  in
  (* Add Expires *)
  let parts =
    match t.expires with
    | Some date -> (String.concat "=" [ "Expires"; date ]) :: parts
    | Option.None -> parts
  in
  (* Add Path *)
  let parts = (String.concat "=" [ "Path"; t.path ]) :: parts in
  (* Add Domain *)
  let parts =
    match t.domain with
    | Some d -> (String.concat "=" [ "Domain"; d ]) :: parts
    | Option.None -> parts
  in
  (* Add Secure flag *)
  let parts =
    if t.secure then
      "Secure" :: parts
    else
      parts
  in
  (* Add HttpOnly flag *)
  let parts =
    if t.http_only then
      "HttpOnly" :: parts
    else
      parts
  in
  (* Add SameSite *)
  let parts =
    match t.same_site with
    | Some ss -> (String.concat "=" [ "SameSite"; same_site_to_string ss ]) :: parts
    | Option.None -> parts
  in
  (* Join with "; " *)
  String.concat "; " (List.reverse parts)

(** Validate cookie name (no special characters) *)
let validate_name = fun name ->
  let len = String.length name in
  if len = 0 then
    Option.some EmptyName
  else
    let rec check i =
      if i >= len then
        Option.none
      else
        let c = String.get_unchecked name ~at:i in
        match c with
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '_'
        | '-' -> check (i + 1)
        | _ -> Option.some (InvalidNameCharacter { index = i; character = c })
    in
    check 0

let is_valid_name = fun name ->
  match validate_name name with
  | Option.None -> true
  | Some _ -> false

(** Validate cookie value (basic check for control characters) *)
let validate_value = fun value ->
  let len = String.length value in
  let rec check i =
    if i >= len then
      Option.none
    else
      let c = String.get_unchecked value ~at:i in
      if Char.to_int c < 32 then
        Option.some (InvalidValueCharacter { index = i; character = c; reason = ControlCharacter })
      else if c = ';' then
        Option.some (InvalidValueCharacter { index = i; character = c; reason = Semicolon })
      else if c = ',' then
        Option.some (InvalidValueCharacter { index = i; character = c; reason = Comma })
      else
        check (i + 1)
  in
  check 0

let is_valid_value = fun value ->
  match validate_value value with
  | Option.None -> true
  | Some _ -> false

let validate_attribute_value = fun attribute value ->
  let len = String.length value in
  let rec check i =
    if i >= len then
      Option.none
    else
      let character = String.get_unchecked value ~at:i in
      let code = Char.to_int character in
      if code < 32 || code = 127 then
        Option.some
          (
            InvalidAttributeCharacter {
              attribute;
              index = i;
              character;
              reason = AttributeControlCharacter;
            }
          )
      else if character = ';' then
        Option.some
          (
            InvalidAttributeCharacter {
              attribute;
              index = i;
              character;
              reason = AttributeSemicolon;
            }
          )
      else
        check (i + 1)
  in
  check 0

let validate_optional_attribute = fun attribute value ->
  match value with
  | Option.None -> Option.none
  | Some value -> validate_attribute_value attribute value

let validate_security = fun ~name ~domain ~path ~secure ~same_site ->
  match (same_site, secure) with
  | (None, false) -> Some SameSiteNoneRequiresSecure
  | (Strict, _)
  | (Lax, _)
  | (None, true) ->
      if String.starts_with ~prefix:"__Secure-" name && not secure then
        Some SecurePrefixRequiresSecure
      else if String.starts_with ~prefix:"__Host-" name && not secure then
        Some HostPrefixRequiresSecure
      else if String.starts_with ~prefix:"__Host-" name && Option.is_some domain then
        Some HostPrefixRequiresNoDomain
      else if String.starts_with ~prefix:"__Host-" name && path != "/" then
        Some HostPrefixRequiresRootPath
      else
        Option.none

(** Create a cookie with validated defaults *)
let make = fun
  ~name
  ~value
  ?max_age
  ?expires
  ?(path = "/")
  ?domain
  ?(secure = false)
  ?(http_only = true)
  ?(same_site = Lax)
  () ->
  let validation_error =
    validate_name name
    |> Option.or_else ~fn:(fun () -> validate_value value)
    |> Option.or_else ~fn:(fun () -> validate_optional_attribute Expires expires)
    |> Option.or_else ~fn:(fun () -> validate_optional_attribute Domain domain)
    |> Option.or_else ~fn:(fun () -> validate_attribute_value Path path)
    |> Option.or_else ~fn:(fun () -> validate_security ~name ~domain ~path ~secure ~same_site)
  in
  match validation_error with
  | Some error -> Error error
  | Option.None ->
      Ok {
        name;
        value;
        max_age;
        expires;
        path;
        domain;
        secure;
        http_only;
        same_site = Some same_site;
      }

(** Create validated cookie *)
let make_validated = make
