(**
   {1 Session Middleware}

   Experimental cookie-based session management for Suri.

   Sessions store small amounts of data (< 4KB) in cookies. No server-side
   storage is required.

   This middleware is not production-ready yet. It still uses placeholder
   signing/encryption internally and must be replaced with authenticated
   encryption before handling sensitive data.

   {2 Quick Start}

   {3 Basic Setup}
   {[
     let app = Middleware.[
       session ~secret:"your-secret-key-256-bits" ();
       router routes;
     ]
   ]}

   {3 Reading Session}
   {[
     let handler ~conn ~next:_ =
       let session = Session.get conn in
       match Session.get_value "user_id" session with
       | Some user_id -> (* Logged in user *)
           conn |> Conn.respond ~status:Ok ~body:"Welcome back!" |> Conn.send
       | None -> (* Anonymous user *)
           conn |> Conn.respond ~status:Ok ~body:"Please login" |> Conn.send
   ]}

   {3 Writing Session}
   {[
     let login_handler ~conn ~next:_ =
       let session = Session.get conn in
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
   - {b Not production safe}: Signing and encryption are placeholders today

   {b Warning}: Current crypto is placeholder-only. Replace it with
   authenticated encryption before production use.

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

(** Abstract session type *)

(**
   Session middleware with experimental cookie storage.

   @param secret Encryption/signing key (required, use 256-bit random value)
   @param cookie_name Cookie name (default: "_suri_session")
   @param max_age Session lifetime in seconds (default: 86400 = 24h)
   @param secure Require HTTPS (default: false, {b set true in production!})
   @param same_site CSRF protection (default: [Lax])

   {b Security Warning}: Always use a strong random secret in production!

   Example:
   {[
     let secret = match Env.get "SESSION_SECRET" with
       | Some s -> s
       | None -> failwith "SESSION_SECRET required"
     in

     Middleware.[
       session ~secret ~secure:true ();
       router routes;
     ]
   ]}
*)
type t

(**
   Get session from connection.

   Creates new session if none exists or cookie invalid.

   Example:
   {[
     let handler ~conn ~next:_ =
       let session = Session.get conn in
       match Session.get_value "user_id" session with
       | Some id -> Printf.printf "User: %s\n" id
       | None -> Printf.printf "Anonymous\n"
   ]}
*)
val middleware:
  secret:string ->
  ?cookie_name:string ->
  ?max_age:int ->
  ?secure:bool ->
  ?same_site:Http.Http1.Cookie.same_site ->
  unit ->
  (conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t)

val get: Conn.t -> t

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
       let session = Session.get conn in
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
