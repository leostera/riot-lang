(**
   {1 CSRF Protection Middleware}

   Cross-Site Request Forgery protection for Suri web applications.

   CSRF attacks trick authenticated users into performing unwanted actions.
   This middleware validates that requests originate from your application
   by checking cryptographic tokens.

   {2 Quick Start}

   {3 Basic Protection}
   {[
     match session ~secret:"0123456789abcdef0123456789abcdef" () with
     | Error error -> Error (Session.setup_error_to_string error)
     | Ok session_middleware ->
         let app = Middleware.[ session_middleware; csrf (); router routes ] in
         Ok app
   ]}

   {3 In HTML Forms}
   {[
     let form_handler ~conn ~next:_ =
       match Csrf.hidden_field conn with
       | Error error ->
           conn
           |> Conn.respond ~status:InternalServerError ~body:(Csrf.error_to_string error)
           |> Conn.send
       | Ok field ->
           let html = String.concat "" [
             "<form method=\"POST\" action=\"/submit\">";
             Component.to_html field;
             "<input name=\"data\" />";
             "<button>Submit</button>";
             "</form>"
           ] in
           conn
           |> Conn.respond ~status:Ok ~body:html
           |> Conn.with_header "content-type" "text/html"
           |> Conn.send
   ]}

   {3 In AJAX Requests}
   {[
     (* In your HTML layout *)
     let layout conn content =
       match Csrf.meta_tag conn with
       | Error error -> Error error
       | Ok tag ->
           Ok (String.concat "" [
             "<html><head>";
             Component.to_html tag;
             "</head><body>";
             content;
             "</body></html>"
           ])

     (* In JavaScript *)
     (* <script>
        const token = document.querySelector('meta[name="csrf-token"]').content;

        fetch('/api/data', {
          method: 'POST',
          headers: {
            'X-CSRF-Token': token,
            'Content-Type': 'application/json'
          },
          body: JSON.stringify(data)
        });
        </script> *)
   ]}

   {2 How It Works}

   1. Requires [Session.middleware] to have run earlier in the pipeline
   2. On first request, generates an OS-seeded random token and stores it in
      session
   3. For unsafe methods (POST, PUT, DELETE), requires token in request
   4. Token can be in parameter ({i _csrf_token}) or header ({i x-csrf-token})
   5. Tokens are masked using XOR to prevent BREACH attacks
   6. Token verification uses constant-time comparison
   7. Failed verification returns 403 Forbidden

   {2 Security Notes}

   - {b Always use HTTPS} in production
   - {b Never disable} for state-changing operations
   - {b Don't expose tokens} in URLs or logs
   - {b Set SameSite=Lax} on session cookies (default in Session middleware)
   - {b Rotate tokens} on login/logout for extra security

   {2 Common Patterns}

   {3 Skip CSRF for API Webhooks}
   {[
     csrf ~skip:(fun conn ->
       String.starts_with ~prefix:"/webhooks" (Conn.path conn)
     ) ()
   ]}

   {3 Custom Token Parameter Name}
   {[
     csrf ~param_name:"authenticity_token" ()
   ]}

   {3 Protect All Methods (Including GET)}
   {[
     csrf ~skip_safe_methods:false ()
   ]}
*)

open Std

type random_error =
  | RngInitializationFailed of Random.error
  | RandomByteFailed of {
      index: int;
      error: Random.error;
    }
type error =
  | MissingSession
  | TokenGenerationFailed of random_error
type unmask_error =
  | InvalidMaskedTokenEncoding
  | InvalidMaskedTokenLength of { expected: int; actual: int }
type verification_error =
  | MissingStoredToken
  | InvalidStoredToken
  | InvalidRequestToken of unmask_error
  | TokenMismatch

val random_error_to_string: random_error -> string

val error_to_string: error -> string

val unmask_error_to_string: unmask_error -> string

val verification_error_to_string: verification_error -> string

val random_bytes_with_rng: Random.Rng.t -> int -> (string, random_error) result

val random_bytes: int -> (string, random_error) result

val generate_token: unit -> (string, error) result

val mask_token: string -> (string, error) result

val unmask_token: string -> (string, unmask_error) result

val get_or_create_token: Session.t -> (string, error) result

val verify_token_result: Session.t -> string -> (unit, verification_error) Std.result

val verify_token: Session.t -> string -> bool

val is_raw_token: string -> bool

val secure_equal: string -> string -> bool

val missing_session_body: string

(**
   CSRF protection middleware.

   @param param_name Form parameter name (default: "_csrf_token")
   @param header_name HTTP header name (default: "x-csrf-token")
   @param skip_safe_methods Skip GET/HEAD/OPTIONS (default: true)
   @param skip Custom skip function for specific paths

   {b Requires}: Session middleware must be earlier in pipeline.

   {b Protected methods}: POST, PUT, PATCH, DELETE, CONNECT, TRACE

   {b Returns}: 403 Forbidden if token missing or invalid

   Example:
   {[
     match session ~secret:"0123456789abcdef0123456789abcdef" () with
     | Error error -> Error (Session.setup_error_to_string error)
     | Ok session_middleware ->
         Ok Middleware.[ session_middleware; csrf (); router routes ]
   ]}
*)
val middleware:
  ?param_name:string ->
  ?header_name:string ->
  ?skip_safe_methods:bool ->
  ?skip:(Conn.t -> bool) ->
  unit ->
  (conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t)

(**
   Get current CSRF token for this request.

   Retrieves token from session, or generates new one if needed.
   Use this in views to get the raw token value.

   Example:
   {[
     match Csrf.get_token conn with
     | Ok token -> (* Use token in custom HTML *)
     | Error error -> (* Render or log Csrf.error_to_string error *)
   ]}
*)
val get_token: Conn.t -> (string, error) result

(**
   Generate HTML hidden input field with CSRF token.

   Returns Component: {i <input type="hidden" name="_csrf_token" value="...">}

   The token is automatically masked for BREACH attack protection.

   Example:
   {[
     let form conn =
       match Csrf.hidden_field conn with
       | Error error -> Error error
       | Ok field ->
           Ok (Component.form_ ~attrs:[Component.method_ "POST"] [
             field;
             Component.input ~attrs:[Component.name "data"] ();
             Component.button [Component.text "Submit"];
           ])
   ]}
*)
val hidden_field: Conn.t -> ('msg Component.t, error) result

(**
   Generate HTML meta tag for AJAX requests.

   Returns Component: {i <meta name="csrf-token" content="...">}

   Place in <head> section of layout for JavaScript access.
   The token is automatically masked for BREACH attack protection.

   Example:
   {[
     let layout conn content =
       match Csrf.meta_tag conn with
       | Error error -> Error error
       | Ok tag ->
           Ok (Component.html [
             Component.head [ tag ];
             Component.body [ content ];
           ])
   ]}
*)
val meta_tag: Conn.t -> ('msg Component.t, error) result
