(**
   {1 HTTP Cookie Support}

   Safe basic cookie parsing and serialization helpers for common RFC 6265
   cookie shapes.

   {2 Quick Examples}

   {3 Parsing Cookie Header}
   {[
     let header = "session_id=abc123; user_pref=dark_mode" in
     let cookies = Cookie.parse header in
     (* [("session_id", "abc123"); ("user_pref", "dark_mode")] *)

     match List.assoc_opt "session_id" cookies with
     | Some value -> (* Use session *)
     | None -> (* No session *)
   ]}

   {3 Creating Set-Cookie Header}
   {[
     match Cookie.make
       ~name:"session_id"
       ~value:"abc123"
       ~max_age:3600
       ~http_only:true
       ~secure:true
       ~same_site:Lax
       () with
     | Ok cookie ->
         let header = Cookie.to_set_cookie cookie in
         (* "session_id=abc123; Max-Age=3600; Path=/; HttpOnly; Secure; SameSite=Lax" *)
         header
     | Error error -> Cookie.validation_error_to_string error
   ]}

   {3 Parsing Set-Cookie Header}
   {[
     let header = "token=xyz; Max-Age=86400; Path=/; Secure" in
     match Cookie.parse_set_cookie header with
     | Some cookie ->
         (* cookie.name = "token" *)
         (* cookie.max_age = Some 86400 *)
     | None -> (* Invalid Set-Cookie *)
   ]}

   {2 SameSite Attribute}

   Controls when cookies are sent with cross-site requests:

   - {b Strict}: Never sent with cross-site requests (most secure)
   - {b Lax}: Sent with top-level navigations (default, recommended)
   - {b None}: Always sent (requires Secure flag)

   {2 Security Best Practices}

   - Always set [http_only:true] for session cookies (prevents XSS)
   - Always set [secure:true] in production (HTTPS only)
   - Use [same_site:Lax] or [Strict] to prevent CSRF
   - Set appropriate [max_age] to limit session lifetime
   - Use [__Host-] or [__Secure-] prefixes for sensitive cookies
*)

open Std

(** {2 Types} *)

type same_site =
  | Strict
  (** Strictest - no cross-site requests *)
  | Lax
  (** Safe cross-site (GET only) - recommended default *)
  | None
(** Allow all cross-site (requires Secure flag) *)
type t = {
  name: string;
  (** Cookie name *)
  value: string;
  (** Cookie value *)
  max_age: int option;
  (** Lifetime in seconds *)
  expires: string option;
  (** HTTP date format expiration *)
  domain: string option;
  (** Domain scope *)
  path: string;
  (** Path scope (default: "/") *)
  secure: bool;
  (** HTTPS only *)
  http_only: bool;
  (** No JavaScript access *)
  same_site: same_site option;
  (** CSRF protection *)
}
type value_character_error =
  | ControlCharacter
  | DeleteCharacter
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
  | InvalidValueCharacter of {
      index: int;
      character: char;
      reason: value_character_error;
    }
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
val validation_error_to_string: validation_error -> string

type parse_set_cookie_error =
  | EmptyHeader
  | MissingNameValueSeparator
  | InvalidMaxAge of max_age_error
  | InvalidSameSite of same_site_error
  | InvalidCookie of validation_error

and max_age_error =
  | EmptyMaxAge
  | NegativeMaxAge
  | MaxAgeOverflow
  | InvalidMaxAgeCharacter of { code: int; index: int }

and same_site_error =
  | EmptySameSite
  | UnknownSameSite of { value: string }
val parse_set_cookie_error_to_string: parse_set_cookie_error -> string

(** {2 Parsing} *)

(**
   Parse Cookie header into name-value pairs.

   {b Example}:
   {[
     parse "a=1; b=2; c=3"  (* [("a", "1"); ("b", "2"); ("c", "3")] *)
   ]}

   Handles:
   - Multiple cookies separated by semicolons
   - Optional whitespace around names/values
   - Missing values (treated as empty string)
*)
val parse: string -> (string * string) list

(**
   Parse Set-Cookie header into cookie record.

   {b Example}:
   {[
     parse_set_cookie "id=a3; Max-Age=3600; Secure"
     (* Some { name = "id"; value = "a3"; max_age = Some 3600; secure = true; ... } *)
   ]}

   Returns [None] if header is malformed.
*)
val parse_set_cookie: string -> t option

(**
   Parse Set-Cookie header into cookie record, preserving structured failure
   information.
*)
val parse_set_cookie_result: string -> (t, parse_set_cookie_error) result

(** {2 Serialization} *)

(**
   Serialize cookie to Set-Cookie header value.

   {b Example}:
   {[
     to_set_cookie { name = "id"; value = "123"; secure = true; ... }
     (* "id=123; Secure; HttpOnly; SameSite=Lax" *)
   ]}

   Automatically includes all set attributes in correct format.
*)
val to_set_cookie: t -> string

(** {2 Construction} *)

(**
   Create a cookie with sensible defaults and validation.

   {b Defaults}:
   - [path]: "/"
   - [http_only]: true (recommended for security)
   - [secure]: false (set true in production!)
   - [same_site]: Some Lax (CSRF protection)

   {b Example}:
   {[
     match make ~name:"session" ~value:"abc123"
             ~max_age:3600 ~secure:true () with
     | Ok cookie -> to_set_cookie cookie
     | Error error -> validation_error_to_string error
   ]}

   Returns [Error error] if validation fails.
*)
val make:
  name:string ->
  value:string ->
  ?max_age:int ->
  ?expires:string ->
  ?path:string ->
  ?domain:string ->
  ?secure:bool ->
  ?http_only:bool ->
  ?same_site:same_site ->
  unit ->
  (t, validation_error) result

(**
   Compatibility alias for [make].

   Validates:
   - Name contains only alphanumeric, underscore, hyphen
   - Value contains no control characters, semicolons, or commas
   - Path, Domain, and Expires contain no control characters or semicolons
   - SameSite=None and cookie prefixes satisfy modern secure-cookie invariants
*)
val make_validated:
  name:string ->
  value:string ->
  ?max_age:int ->
  ?expires:string ->
  ?path:string ->
  ?domain:string ->
  ?secure:bool ->
  ?http_only:bool ->
  ?same_site:same_site ->
  unit ->
  (t, validation_error) result

(** {2 Validation} *)

(** Check if cookie name is valid (alphanumeric + underscore + hyphen). *)
val is_valid_name: string -> bool

(** Check if cookie value is valid (no control characters). *)
val is_valid_value: string -> bool

(** {2 Utilities} *)

(** Convert SameSite to string ("Strict", "Lax", or "None"). *)
val same_site_to_string: same_site -> string
