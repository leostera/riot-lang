open Std

(** {1 HTTP Basic Authentication Middleware}

    Simple HTTP Basic Authentication for protecting routes with username/password.

    {b ⚠️ SECURITY WARNING}
    
    HTTP Basic Authentication transmits credentials in Base64 encoding,
    which is {b NOT encryption}. Credentials are visible to anyone who can
    intercept the connection.
    
    {b ALWAYS use HTTPS in production!}
    
    Basic Auth is suitable for:
    - Development/testing environments
    - Internal tools (over VPN/HTTPS)
    - Quick prototypes (over HTTPS)
    - Admin panels (over HTTPS)
    
    For production authentication, consider:
    - OAuth2 / OpenID Connect
    - Session-based auth (see {!Session})
    - JWT tokens

    {2 Quick Start}

    Simple username/password protection:
    {[
      let app = Middleware.[
        logger;
        basic_auth ~username:"admin" ~password:"secret" ();
        router routes;
      ]
    ]}

    With custom realm:
    {[
      let app = Middleware.[
        logger;
        basic_auth 
          ~username:"admin" 
          ~password:"secret" 
          ~realm:"Admin Panel" 
          ();
        router routes;
      ]
    ]}

    {2 Custom Validation}

    For database lookups or complex validation:
    {[
      let validate ~username ~password =
        match Database.find_user username with
        | Some user when verify_password user password -> Some user
        | _ -> None
      in
      
      let app = Middleware.[
        logger;
        basic_auth_with_validation ~validate ~realm:"Member Area" ();
        router routes;
      ]
    ]}

    Access authenticated user in handlers:
    {[
      let handler ~conn ~next:_ =
        match Basic_auth.get "user" conn with
        | Some user -> 
            Printf.sprintf "Welcome, %s!" user.username
            |> Conn.respond conn ~status:Ok ~body:_
            |> Conn.send
        | None -> 
            Conn.respond conn ~status:Unauthorized ~body:"Unauthorized"
            |> Conn.send
    ]}

    {2 Skip Paths}

    Allow public access to specific paths:
    {[
      let app = Middleware.[
        logger;
        basic_auth 
          ~username:"admin" 
          ~password:"secret"
          ~skip:(fun conn ->
            let path = Conn.request_path conn in
            String.starts_with path ~prefix:"/public" ||
            String.starts_with path ~prefix:"/health"
          )
          ();
        router routes;
      ]
    ]}

    {2 Security Features}

    - ✅ Constant-time password comparison (timing attack prevention)
    - ✅ Realm sanitization (header injection prevention)
    - ✅ RFC 7617 compliant
    - ✅ Works with existing middleware (CORS, CSRF, etc.)

    {2 How It Works}

    1. Client makes request without Authorization header
    2. Server responds with [401 Unauthorized] + [WWW-Authenticate: Basic realm="..."]
    3. Browser prompts for username/password
    4. Client retries with [Authorization: Basic <base64-credentials>]
    5. Server validates and either:
       - Returns requested resource (200 OK)
       - Returns 401 again (invalid credentials) *)

(** {1 Types} *)

type 'a validation_fn = username:string -> password:string -> 'a option
(** Validation function type.
    
    Return [Some value] on successful authentication, [None] on failure.
    The [value] will be stored in the connection under the key ["basic_auth_user"]
    and can be retrieved with {!get}.
    
    Example:
    {[
      let validate ~username ~password =
        match Database.find_user username with
        | Some user when verify_password user password -> Some user
        | _ -> None
    ]} *)

(** {1 Middleware} *)

val middleware :
  ?realm:string ->
  ?skip:(Conn.t -> bool) ->
  username:string ->
  password:string ->
  unit ->
  Pipeline.middleware
(** Create Basic Auth middleware with static credentials.
    
    @param realm Realm name shown in browser prompt (default: "Restricted Area")
    @param skip Function to skip authentication for specific requests
    @param username Expected username
    @param password Expected password
    
    Example:
    {[
      let app = Middleware.[
        logger;
        basic_auth ~username:"admin" ~password:"secret" ();
        router routes;
      ]
    ]}
    
    With custom realm:
    {[
      basic_auth 
        ~username:"admin" 
        ~password:"secret" 
        ~realm:"Admin Panel" 
        ()
    ]}
    
    Skip public paths:
    {[
      basic_auth 
        ~username:"admin" 
        ~password:"secret"
        ~skip:(fun conn ->
          String.starts_with (Conn.request_path conn) ~prefix:"/public"
        )
        ()
    ]} *)

val middleware_with_validation :
  ?realm:string ->
  ?skip:(Conn.t -> bool) ->
  validate:'a validation_fn ->
  unit ->
  Pipeline.middleware
(** Create Basic Auth middleware with custom validation.
    
    Use this for database lookups, LDAP authentication, or any custom
    validation logic.
    
    @param realm Realm name shown in browser prompt (default: "Restricted Area")
    @param skip Function to skip authentication for specific requests
    @param validate Function to validate credentials and return user data
    
    Example:
    {[
      let validate ~username ~password =
        match Database.find_user username with
        | Some user when verify_password user password -> Some user
        | _ -> None
      in
      
      let app = Middleware.[
        logger;
        basic_auth_with_validation ~validate ~realm:"Member Area" ();
        router routes;
      ]
    ]}
    
    The validated user data is stored in the connection:
    {[
      let handler ~conn ~next:_ =
        match Basic_auth.get "user" conn with
        | Some user -> (* user is what validate returned *)
        | None -> (* should never happen if middleware passed *)
    ]} *)

(** {1 Helper Functions} *)

val get_credentials : Conn.t -> (string * string) option
(** Extract username and password from Authorization header.
    
    Returns [Some (username, password)] if Authorization header is present
    and valid, [None] otherwise.
    
    Example:
    {[
      match Basic_auth.get_credentials conn with
      | Some (username, password) -> 
          Printf.printf "User: %s\n" username
      | None -> 
          print_endline "No credentials provided"
    ]}
    
    This is exposed for advanced use cases. Most applications should use
    {!middleware} or {!middleware_with_validation} instead. *)

val assign : string -> 'a -> Conn.t -> Conn.t
(** Store authenticated user data in connection.
    
    This is called automatically by {!middleware_with_validation}.
    Use this if you need to store additional authentication data.
    
    Example:
    {[
      let conn = Basic_auth.assign "user_role" "admin" conn in
      (* Later... *)
      match Basic_auth.get "user_role" conn with
      | Some role -> Printf.printf "Role: %s\n" role
      | None -> ()
    ]} *)

val get : string -> Conn.t -> 'a option
(** Get authenticated user data from connection.
    
    Retrieve data stored by {!middleware_with_validation} or {!assign}.
    
    Example:
    {[
      match Basic_auth.get "basic_auth_user" conn with
      | Some user -> Printf.printf "Welcome, %s!\n" user.username
      | None -> print_endline "Not authenticated"
    ]}
    
    {b Note}: The default key used by {!middleware_with_validation} is
    ["basic_auth_user"]. *)
