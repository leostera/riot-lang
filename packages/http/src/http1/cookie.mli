(** {1 HTTP Cookie Support}
    
    RFC 6265 compliant cookie parsing and serialization.
    
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
      let cookie = Cookie.make
        ~name:"session_id"
        ~value:"abc123"
        ~max_age:3600
        ~http_only:true
        ~secure:true
        ~same_site:Lax
        ()
      in
      
      let header = Cookie.to_set_cookie cookie in
      (* "session_id=abc123; Max-Age=3600; HttpOnly; Secure; SameSite=Lax" *)
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
  | Strict (** Strictest - no cross-site requests *)
  | Lax (** Safe cross-site (GET only) - recommended default *)
  | None (** Allow all cross-site (requires Secure flag) *)
type t = {
  name : string;
  (** Cookie name *)
  value : string;
  (** Cookie value *)
  max_age : int option;
  (** Lifetime in seconds *)
  expires : string option;
  (** HTTP date format expiration *)
  domain : string option;
  (** Domain scope *)
  path : string;
  (** Path scope (default: "/") *)
  secure : bool;
  (** HTTPS only *)
  http_only : bool;
  (** No JavaScript access *)
  same_site : same_site option;
  (** CSRF protection *)
}
(** {2 Parsing} *)

(** Parse Cookie header into name-value pairs.
    
    {b Example}:
    {[
      parse "a=1; b=2; c=3"  (* [("a", "1"); ("b", "2"); ("c", "3")] *)
    ]}
    
    Handles:
    - Multiple cookies separated by semicolons
    - Optional whitespace around names/values
    - Missing values (treated as empty string) *)
val parse : string -> (string * string) list

(** Parse Set-Cookie header into cookie record.
    
    {b Example}:
    {[
      parse_set_cookie "id=a3; Max-Age=3600; Secure"
      (* Some { name = "id"; value = "a3"; max_age = Some 3600; secure = true; ... } *)
    ]}
    
    Returns [None] if header is malformed. *)
val parse_set_cookie : string -> t option

(** {2 Serialization} *)

(** Serialize cookie to Set-Cookie header value.
    
    {b Example}:
    {[
      to_set_cookie { name = "id"; value = "123"; secure = true; ... }
      (* "id=123; Secure; HttpOnly; SameSite=Lax" *)
    ]}
    
    Automatically includes all set attributes in correct format. *)
val to_set_cookie : t -> string

(** {2 Construction} *)

(** Create a cookie with sensible defaults.
    
    {b Defaults}:
    - [path]: "/"
    - [http_only]: true (recommended for security)
    - [secure]: false (set true in production!)
    - [same_site]: Some Lax (CSRF protection)
    
    {b Example}:
    {[
      make ~name:"session" ~value:"abc123" 
           ~max_age:3600 ~secure:true ()
    ]} *)
val make : name:string ->
value:string ->
?max_age:int ->
?expires:string ->
?path:string ->
?domain:string ->
?secure:bool ->
?http_only:bool ->
?same_site:same_site ->
unit ->
t

(** Create a cookie with validation.
    
    Validates:
    - Name contains only alphanumeric, underscore, hyphen
    - Value contains no control characters or semicolons
    
    Returns [Error msg] if validation fails. *)
val make_validated : name:string ->
value:string ->
?max_age:int ->
?expires:string ->
?path:string ->
?domain:string ->
?secure:bool ->
?http_only:bool ->
?same_site:same_site ->
unit ->
(t, string) result

(** {2 Validation} *)

(** Check if cookie name is valid (alphanumeric + underscore + hyphen). *)
val is_valid_name : string -> bool

(** Check if cookie value is valid (no control characters). *)
val is_valid_value : string -> bool

(** {2 Utilities} *)

(** Convert SameSite to string ("Strict", "Lax", or "None"). *)
val same_site_to_string : same_site -> string
