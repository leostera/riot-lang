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

(** Parse Cookie header: "name1=value1; name2=value2" *)
let parse = fun header ->
  String.split ~by:";" header |> List.filter_map
    ~fn:(fun pair ->
      let trimmed = String.trim pair in
      match String.split ~by:"=" trimmed with
      | [] ->
          Option.none
      | [ name ] ->
          Some (String.trim name, "")
      | name :: value_parts ->
          let value = String.concat "=" value_parts in
          Some (String.trim name, String.trim value))

(** Helper: Parse Set-Cookie attribute *)
let parse_attribute = fun attr ->
  let trimmed = String.trim attr in
  match String.split ~by:"=" trimmed with
  | [] ->
      (Option.none, Option.none)
  | [ key ] ->
      (Some (String.lowercase_ascii (String.trim key)), Option.none)
  | key :: value_parts ->
      let value = String.concat "=" value_parts in
      (Some (String.lowercase_ascii (String.trim key)), Some (String.trim value))

(** Parse Set-Cookie header *)
let parse_set_cookie = fun header ->
  match String.split ~by:";" header with
  | [] -> Option.none
  | first :: attrs -> (* Parse name=value *)
    (
      match String.split ~by:"=" first with
      | name :: value_parts ->
          let cookie = {
            name = String.trim name;
            value = String.concat "=" value_parts |> String.trim;
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
            List.fold_left attrs
              ~acc:cookie
              ~fn:(fun c attr ->
                match parse_attribute attr with
                | (Some "max-age", Some value) -> (
                    match Int.parse value with
                    | Some age -> { c with max_age = Some age }
                    | Option.None -> c
                  )
                | (Some "expires", Some value) ->
                    { c with expires = Some value }
                | (Some "path", Some value) ->
                    { c with path = value }
                | (Some "domain", Some value) ->
                    { c with domain = Some value }
                | (Some "secure", Option.None) ->
                    { c with secure = true }
                | (Some "httponly", Option.None) ->
                    { c with http_only = true }
                | (Some "samesite", Some value) ->
                    let same_site =
                      match String.lowercase_ascii value with
                      | "strict" -> Some Strict
                      | "lax" -> Some Lax
                      | "none" -> Some None
                      | _ -> Option.none
                    in
                    { c with same_site }
                | _ ->
                    c)
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
  (* Add Path (only if not default) *)
  let parts =
    if t.path = "/" then
      parts
    else
      (String.concat "=" [ "Path"; t.path ]) :: parts
  in
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

(** Create a cookie with defaults *)
let make = fun ~name ~value ?max_age ?expires ?(path = "/") ?domain ?(secure = false) ?(http_only = true) ?(same_site = Lax) () ->
  {
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

(** Validate cookie name (no special characters) *)
let is_valid_name = fun name ->
  let len = String.length name in
  if len = 0 then
    false
  else
    let rec check i =
      if i >= len then
        true
      else
        let c = String.get_unchecked name ~at:i in
        match c with
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '_'
        | '-' -> check (i + 1)
        | _ -> false
    in
    check 0

(** Validate cookie value (basic check for control characters) *)
let is_valid_value = fun value ->
  let len = String.length value in
  let rec check i =
    if i >= len then
      true
    else
      let c = String.get_unchecked value ~at:i in
      if Char.to_int c < 32 || c = ';' || c = ',' then
        false
      else
        check (i + 1)
  in
  check 0

(** Create validated cookie *)
let make_validated = fun ~name ~value ?max_age ?expires ?path ?domain ?secure ?http_only ?same_site () ->
  if not (is_valid_name name) then
    Error (String.concat "" [ "Invalid cookie name: "; name ])
  else if not (is_valid_value value) then
    Error "Invalid cookie value (contains control characters)"
  else
    Ok (make ~name ~value ?max_age ?expires ?path ?domain ?secure ?http_only ?same_site ())
