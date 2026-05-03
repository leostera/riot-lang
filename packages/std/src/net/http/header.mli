(**
   HTTP headers.

   HTTP header manipulation with case-insensitive names and support for
   multiple values per header name.

   ## Examples

   Creating and manipulating headers:

   ```ocaml open Std.Net.Http

   let headers = Header.empty in let headers = Header.set headers
   "Content-Type" "application/json" in let headers = Header.set headers
   "Accept" "application/json" in

   (* Get header value *) match Header.get headers "Content-Type" with | Some
   ct -> Printf.printf "Content-Type: %s\n" ct | None -> ()

   (* Headers are case-insensitive *) Header.get headers "content-type" (* Some
   "application/json" *) ```

   Multiple values per header:

   ```ocaml let headers = Header.empty in let headers = Header.add headers
   "Accept" "text/html" in let headers = Header.add headers "Accept"
   "application/json" in

   (* Get all values *) Header.get_all headers "Accept" (*
   ["text/html"; "application/json"] *)

   (* Get first value *) Header.get headers "Accept" (* Some "text/html" *) ```

   Using common header names:

   ```ocaml let headers = Header.empty in let headers = Header.set headers
   Header.Name.content_type "text/plain" in let headers = Header.set headers
   Header.Name.authorization "Bearer token123" in

   Header.get headers Header.Name.content_type (* Some "text/plain" *) ```

   Parsing header values:

   ```ocaml let ct = "application/json; charset=utf-8" in match
   Header.Value.parse_content_type ct with | Ok (media_type, params) -> (*
   media_type = "application/json" *) (* params = [("charset", "utf-8")] *) |
   Error InvalidContentType -> () ```
*)
open Kernel
open Global

(** Header name (case-insensitive) *)
type name = string
(** Header value *)
type value = string
(** Collection of HTTP headers *)
type t

(**
   Creates an empty header collection.

   ## Examples

   ```ocaml let headers = Header.empty in assert (Header.is_empty headers) ```
*)
val empty: t

(**
   Creates headers from a list of name-value pairs.

   ## Examples

   ```ocaml let headers = Header.of_list
   [ ("Content-Type", "text/html"); ("Accept", "text/html") ] ```
*)
val of_list: (name * value) list -> t

(**
   Converts headers to a list of name-value pairs.

   ## Examples

   ```ocaml let headers = Header.of_list [("Host", "example.com")] in
   Header.to_list headers (* [("host", "example.com")] *) ```
*)
val to_list: t -> (name * value) list

(**
   Adds a header, allowing multiple values for the same name.

   ## Examples

   ```ocaml let headers = Header.empty in let headers = Header.add headers
   "Accept" "text/html" in let headers = Header.add headers "Accept"
   "application/json" in

   Header.get_all headers "Accept" (* ["text/html"; "application/json"] *) ```
*)
val add: t -> name -> value -> t

(**
   Sets a header, replacing any existing values for that name.

   ## Examples

   ```ocaml let headers = Header.empty in let headers = Header.set headers
   "Host" "old.com" in let headers = Header.set headers "Host" "new.com" in

   Header.get headers "Host" (* Some "new.com" *) ```
*)
val set: t -> name -> value -> t

(**
   Removes all headers with the given name.

   ## Examples

   ```ocaml let headers = Header.set Header.empty "Host" "example.com" in let
   headers = Header.remove headers "Host" in

   Header.has headers "Host" (* false *) ```
*)
val remove: t -> name -> t

(**
   Returns the first value for the given header name.

   ## Examples

   ```ocaml Header.get headers "Content-Type" (* Some "text/html" *) Header.get
   headers "Missing" (* None *)

   (* Case-insensitive *) Header.get headers "content-type" (* Some "text/html"
   *) ```
*)
val get: t -> name -> value option

(**
   Returns all values for the given header name.

   ## Examples

   ```ocaml let headers = Header.empty |> Header.add "Accept" "text/html" |>
   Header.add "Accept" "application/json" in

   Header.get_all headers "Accept" (* ["text/html"; "application/json"] *) ```
*)
val get_all: t -> name -> value list

(**
   Checks if a header with the given name exists.

   ## Examples

   ```ocaml Header.has headers "Content-Type" (* true *) Header.has headers
   "Missing" (* false *) ```
*)
val has: t -> name -> bool

(**
   Applies function to each header name-value pair.

   ## Examples

   ```ocaml Header.iter (fun name value -> Printf.printf "%s: %s\n" name value
   ) headers ```
*)
val iter: (name -> value -> unit) -> t -> unit

(**
   Folds over all header name-value pairs.

   ## Examples

   ```ocaml let count = Header.fold (fun _ _ acc -> acc + 1) headers 0 in ```
*)
val fold: (name -> value -> 'a -> 'a) -> t -> 'a -> 'a

(**
   Returns the number of header entries (including duplicates).

   ## Examples

   ```ocaml let headers = Header.empty |> Header.add "Accept" "text/html" |>
   Header.add "Accept" "application/json" in

   Header.length headers (* 2 *) ```
*)
val length: t -> int

(**
   Checks if headers collection is empty.

   ## Examples

   ```ocaml Header.is_empty Header.empty (* true *) ```
*)
val is_empty: t -> bool

module Name: sig
  (**
     Standard HTTP header name constants. Using these ensures correct spelling
     and consistency.

     ## Examples

     ```ocaml Header.set headers Header.Name.content_type "text/html"
     Header.set headers Header.Name.authorization "Bearer token" ```
  *)
  val content_type: name

  (** "Content-Type" - Media type of the resource *)
  val content_length: name

  (** "Content-Length" - Size of the resource in bytes *)
  val authorization: name

  (** "Authorization" - Authentication credentials *)
  val user_agent: name

  (** "User-Agent" - Client software information *)
  val accept: name

  (** "Accept" - Acceptable media types *)
  val accept_encoding: name

  (** "Accept-Encoding" - Acceptable content encodings *)
  val accept_language: name

  (** "Accept-Language" - Acceptable languages *)
  val cache_control: name

  (** "Cache-Control" - Caching directives *)
  val connection: name

  (** "Connection" - Connection options *)
  val cookie: name

  (** "Cookie" - HTTP cookies *)
  val host: name

  (** "Host" - Target host and port *)
  val referer: name

  (** "Referer" - Previous page URL *)
  val server: name

  (** "Server" - Server software information *)
  val set_cookie: name

  (** "Set-Cookie" - Set HTTP cookies *)
  val transfer_encoding: name

  (** "Transfer-Encoding" - Transfer encoding method *)
  val location: name

  (** "Location" - Redirect target URL *)
  val www_authenticate: name

  (** "WWW-Authenticate" - Authentication method *)
  val date: name

  (** "Date" - Message origination date/time *)
  val etag: name

  (** "ETag" - Entity tag for cache validation *)
  val expires: name

  (** "Expires" - Expiration date/time *)
  val last_modified: name

  (** "Last-Modified" - Last modification date/time *)
  val if_modified_since: name

  (** "If-Modified-Since" - Conditional request *)
  val if_none_match: name

  (** "If-None-Match" - Conditional request with ETag *)
  val vary: name

  (** "Vary" - Variance in content negotiation *)
  val x_forwarded_for: name

  (** "X-Forwarded-For" - Original client IP (proxy) *)
  val x_real_ip: name

  (** "X-Real-IP" - Original client IP (nginx) *)
end

module Value: sig
  type content_type_error =
    | InvalidContentType
  type authorization_error =
    | InvalidAuthorization

  val parse_content_type: value -> (string * (string * string) list, content_type_error) result

  (**
     Parses Authorization header into scheme and credentials.

     ## Examples

     ```ocaml Header.Value.parse_authorization "Bearer abc123" (* Ok ("Bearer",
     "abc123") *)

     Header.Value.parse_authorization "Basic dXNlcjpwYXNz" (* Ok ("Basic",
     "dXNlcjpwYXNz") *) ```
  *)
  val parse_authorization: value -> (string * string, authorization_error) result

  val parse_cache_control: value -> (string * string option) list

  (**
     Parses Cache-Control directives into list of (directive, value) pairs.

     ## Examples

     ```ocaml Header.Value.parse_cache_control "max-age=3600, must-revalidate"
     (* [("max-age", Some "3600"); ("must-revalidate", None)] *) ```
  *)
  val parse_accept: value -> (string * float option * (string * string) list) list

  (**
     Parses Accept header with quality values and parameters. Returns list of
     (media_type, quality, parameters).

     ## Examples

     ```ocaml Header.Value.parse_accept "text/html;q=0.9, application/json" (*
     [("text/html", Some 0.9, []); ("application/json", None, [])] *) ```
  *)
end
