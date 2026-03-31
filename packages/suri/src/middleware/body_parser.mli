(** # Body Parser Middleware

    Automatically parses request bodies based on Content-Type header and stores
    parsed data in [Conn.body_params].

    ## Supported Content Types

    - [application/x-www-form-urlencoded] - HTML forms
    - [application/json] - JSON payloads  
    - [multipart/form-data] - File uploads (TODO: Phase 2)

    ## Example

    Basic usage with defaults (urlencoded + JSON, 10MB limit):
    {[
      let app = Middleware.[
        body_parser ();  (* Parse body before CSRF *)
        csrf ();
        router routes;
      ]
    ]}

    Custom configuration:
    {[
      let app = Middleware.[
        body_parser ~config:{
          parsers = [Urlencoded; Json];
          max_body_size = 50 * 1024 * 1024;  (* 50MB *)
        } ();
        router routes;
      ]
    ]}

    In handlers:
    {[
      let create_user conn =
        let name = List.assoc_opt "name" (Conn.body_params conn) in
        let email = List.assoc_opt "email" (Conn.body_params conn) in
        (* ... *)
    ]} *)

type parser =
  | Urlencoded
  (** application/x-www-form-urlencoded *)
  | Json
  (** application/json *)
  | Multipart
(** multipart/form-data *)
type config = {
  parsers : parser list;
  (** List of enabled parsers (default: [Urlencoded; Json]) *)
  max_body_size : int;
  (** Maximum body size in bytes (default: 10MB). Bodies exceeding this are
          not parsed. *)
}
val default_config : unit -> config

(** Default configuration: urlencoded and JSON parsing, 10MB limit *)
val make : ?config:config -> unit -> conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t

(** Create body parser middleware.

    The middleware reads [Content-Type] header and parses the body accordingly:
    
    - URL-encoded bodies are parsed using {!Net.Uri.Query.parse}
    - JSON bodies with object root are converted to string pairs
    - Multipart bodies are reserved for future implementation
    
    Parsed data is stored in [Conn.body_params] for access by handlers and
    downstream middleware (e.g., CSRF protection).
    
    Bodies larger than [max_body_size] are skipped without parsing. *)
