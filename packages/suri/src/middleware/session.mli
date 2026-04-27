(**
   {1 Session Middleware}

   Experimental cookie-based session management for Suri.

   Sessions store small amounts of data (< 4KB) in cookies. No server-side
   storage is required.

   This middleware is not production-ready yet. Cookies are integrity-protected
   with HMAC-SHA256, but payloads are signed plaintext and must not contain
   secrets or sensitive values.

   {2 Quick Start}

   {3 Basic Setup}
   {[
     match session ~secret:"0123456789abcdef0123456789abcdef" () with
     | Error error -> Error (Session.secret_error_to_string error)
     | Ok session_middleware ->
         let app = Middleware.[ session_middleware; router routes ] in
         Ok app
   ]}

   {3 Reading Session}
   {[
     let handler ~conn ~next:_ =
       match Session.get conn with
       | Some session when Option.is_some (Session.get_value "user_id" session) ->
           conn |> Conn.respond ~status:Ok ~body:"Welcome back!" |> Conn.send
       | Some _
       | None ->
           conn |> Conn.respond ~status:Ok ~body:"Please login" |> Conn.send
   ]}

   {3 Writing Session}
   {[
     let login_handler ~conn ~next:_ =
       match Session.get conn with
       | None ->
           conn |> Conn.respond ~status:InternalServerError ~body:"Session missing" |> Conn.send
       | Some session ->
           Session.put "user_id" "123" session;
           Session.put "username" "alice" session;

           conn
           |> Conn.respond ~status:Ok ~body:"Logged in!"
           |> Conn.send
   ]}

   {2 Security Status}

   - {b Secret Key}: Use 256-bit random secret in production
   - {b HTTPS Only}: Set [~secure:true] in production
   - {b SameSite}: Default [Lax] helps reduce CSRF exposure
   - {b HttpOnly}: Always enabled, prevents XSS access
   - {b Integrity}: Cookie payloads are signed with HMAC-SHA256
   - {b Confidentiality}: Payloads are not encrypted; do not store secrets

   {b Warning}: Add authenticated encryption before storing confidential
   session data in cookies.

   {2 Configuration}

   {[
     session
       ~secret:"production-secret-256-bits"
       ~cookie_name:"_app_session"
       ~max_age:86400  (* 24 hours *)
       ~secure:true
       ~same_site:Http1.Cookie.Strict
       ()
   ]}

   {2 Best Practices}

   1. Generate a strong random secret:
      {v openssl rand -hex 32 v}

   2. Store secret in environment variables, never in code

   3. Always use HTTPS in production ([~secure:true])

   4. Set appropriate session lifetime with [~max_age]

   5. Use [SameSite=Strict] for sensitive applications

   6. Keep session data small (< 4KB cookie limit)
*)

open Std

(** Abstract session type. *)
type t

type secret_error =
  | Missing
  | TooShort of int
type decode_error =
  | InvalidCookieFormat of { parts: int }
  | InvalidSignature
  | InvalidPayloadBase64
  | InvalidJson of Data.Json.error
  | InvalidSessionData of Data.Json.t

(**
   Session middleware with experimental cookie storage.

   @param secret Signing key (required, at least 32 characters)
   @param cookie_name Cookie name (default: "_suri_session")
   @param max_age Session lifetime in seconds (default: 86400 = 24h)
   @param secure Require HTTPS (default: false, {b set true in production!})
   @param same_site CSRF protection (default: [Lax])

   Returns [Error] when the provided secret is missing or too short.

   Example:
   {[
     let secret = match Env.get "SESSION_SECRET" with
       | Some s -> s
       | None -> failwith "SESSION_SECRET required"
     in

     match session ~secret ~secure:true () with
     | Error error -> Error (Session.secret_error_to_string error)
     | Ok session_middleware ->
         Ok Middleware.[ session_middleware; router routes ]
   ]}
*)
val middleware:
  secret:string ->
  ?cookie_name:string ->
  ?max_age:int ->
  ?secure:bool ->
  ?same_site:Http.Http1.Cookie.same_site ->
  unit ->
  ((conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t), secret_error) result

(**
   Get session from connection.

   Returns [None] when [Session.middleware] has not run yet.
*)
val get: Conn.t -> t option

(**
   Find session from connection.

   Returns [None] when [Session.middleware] has not run yet.
*)
val find: Conn.t -> t option

(**
   Get value from session by key.

   Returns [None] if key doesn't exist.

   Example:
   {[
     match Session.get_value "user_id" session with
     | Some id -> (* Use ID *)
     | None -> (* No user ID *)
   ]}
*)
val get_value: string -> t -> string option

(**
   Set value in session. Marks session as modified.

   Session will be saved to cookie on response.

   Example:
   {[
     Session.put "user_id" "123" session;
     Session.put "username" "alice" session;
   ]}
*)
val put: string -> string -> t -> unit

(**
   Delete value from session by key.

   Marks session as modified.

   Example:
   {[
     Session.delete "user_id" session
   ]}
*)
val delete: string -> t -> unit

(**
   Clear all session data.

   Useful for logout - removes all keys but keeps session.

   Example:
   {[
     let logout_handler ~conn ~next:_ =
       match Session.get conn with
       | None ->
           conn |> Conn.respond ~status:InternalServerError ~body:"Session missing" |> Conn.send
       | Some session ->
           Session.clear session;
           conn |> Conn.respond ~status:Ok ~body:"Logged out" |> Conn.send
   ]}
*)
val clear: t -> unit

(**
   Check if session is expired.

   Automatically handled by middleware, but can be useful
   for manual session validation.
*)
val is_expired: t -> bool

(**
   Check if session was modified.

   Used internally to determine if cookie needs updating.
*)
val is_modified: t -> bool

(** Create an empty session after validating the signing secret. *)
val create: cookie_name:string -> secret:string -> unit -> (t, secret_error) result

val validate_secret: string -> (unit, secret_error) result

val secret_error_to_string: secret_error -> string

val decode_error_to_string: decode_error -> string

val sign: secret:string -> string -> string

val verify: secret:string -> string -> string -> bool

val to_cookie_value: t -> string

val cookie_value_for_plaintext: secret:string -> string -> string

val from_cookie_value: cookie_name:string -> secret:string -> string -> (t, decode_error) result
