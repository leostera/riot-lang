(**
   # Body Parser Middleware

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
   ]}
*)

type parser =
  | Urlencoded
  (** application/x-www-form-urlencoded *)
  | Json
  (** application/json *)
  | Multipart
(** multipart/form-data *)
type config = {
  parsers: parser list;
  (** List of enabled parsers (default: [Urlencoded; Json]) *)
  max_body_size: int;
  (**
     Maximum body size in bytes (default: 10MB). Bodies exceeding this are
     not parsed.
  *)
}
type json_root_kind =
  | JsonNull
  | JsonBool
  | JsonInt
  | JsonFloat
  | JsonString
  | JsonArray
  | JsonObject
type parse_error =
  | BodyTooLarge of { size: int; max_size: int }
  (** Request body exceeded the configured limit. *)
  | InvalidContentType of { value: string }
  (** Content-Type header could not be parsed. *)
  | InvalidJson of Std.Data.Json.error
  (** JSON parser returned a structured syntax error. *)
  | JsonRootNotObject of json_root_kind
  (** JSON parsed successfully, but the root value cannot populate body params. *)
  | MissingMultipartBoundary

(** Multipart request did not provide a boundary parameter. *)
val default_config: unit -> config

(** Default configuration: urlencoded and JSON parsing, 10MB limit *)
val parse_error_to_string: parse_error -> string

(** Render a parse error for logs or plain-text client responses. *)
val parse_body:
  config ->
  content_type:string ->
  body:string ->
  ((string * string) list, parse_error) Std.result

(**
   Parse a body for a known Content-Type. Unsupported enabled parsers return an
   empty parameter list; malformed known bodies return a structured error.
*)
val make: ?config:config -> unit -> conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t

(**
   Create body parser middleware.

   The middleware reads [Content-Type] header and parses the body accordingly:

   - URL-encoded bodies are parsed using {!Net.Uri.Query.parse}
   - JSON bodies with object root are converted to string pairs
   - Multipart bodies are reserved for future implementation

   Parsed data is stored in [Conn.body_params] for access by handlers and
   downstream middleware (e.g., CSRF protection).

   Bodies larger than [max_body_size] return [413 Payload Too Large]. Malformed
   JSON or multipart metadata returns [400 Bad Request] with a plain-text
   description.
*)
module For_testing: sig
  val parse_body:
    config ->
    content_type:string ->
    body:string ->
    ((string * string) list, parse_error) Std.result
end
