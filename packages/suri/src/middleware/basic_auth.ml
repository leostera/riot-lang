open Std

(** {1 Security Helpers} *)

(**
   Constant-time string equality check.

   Prevents timing attacks by always comparing the full length of both strings
   and using bitwise operations instead of short-circuit boolean logic.
*)
let secure_equal = fun s1 s2 ->
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  (* Always compare full lengths to prevent length-based timing *)
  let result = ref 0 in
  for i = 0 to max len1 len2 - 1 do
    let c1 =
      if i < len1 then
        Char.code (String.get_unchecked s1 ~at:i)
      else
        0
    in
    let c2 =
      if i < len2 then
        Char.code (String.get_unchecked s2 ~at:i)
      else
        0
    in
    result := !result lor (c1 lxor c2)
  done;
  (* Length difference also contributes *)
  result := !result lor (len1 lxor len2);
  !result = 0

(**
   Sanitize realm value to prevent header injection.

   Removes characters that could be used to inject additional headers:
   - carriage return
   - newline
   - quotes (interfere with realm quoting)
*)
let sanitize_realm = fun realm ->
  let chars = ref [] in
  String.iter
    (fun c ->
      if not (c = '\r' || c = '\n' || c = '"') then
        chars := c :: !chars)
    realm;
  let chars = List.rev !chars in
  let bytes = IO.Bytes.create ~size:(List.length chars) in
  chars
  |> List.enumerate
  |> List.for_each
    ~fn:(fun (index, char) ->
      let _ = IO.Bytes.set_unchecked bytes ~at:index ~char in
      ());
  String.from_bytes bytes

(** {1 Credential Parsing} *)

type credential_decode_error =
  | InvalidAuthorizationFormat
  | InvalidBase64Credentials
  | MissingCredentialSeparator

let credential_decode_error_to_string = function
  | InvalidAuthorizationFormat -> "invalid Basic authorization header format"
  | InvalidBase64Credentials -> "invalid Basic authorization credentials encoding"
  | MissingCredentialSeparator -> "Basic authorization credentials must contain ':'"

let decode_basic_payload = fun encoded ->
  match Encoding.Base64.decode encoded with
  | Result.Ok decoded -> Ok decoded
  | Result.Error Encoding.Base64.InvalidBase64 -> Error InvalidBase64Credentials

(**
   Extract and decode credentials from Authorization header.

   Parses "Authorization: Basic <base64>" header and decodes to
   (username, password) tuple.

   Handles passwords containing colons correctly by only splitting on
   the first colon.
*)
let decode_credentials = fun auth_header ->
  (* Split "Basic <encoded>" *)
  let parts =
    String.split_on_char ' ' auth_header
    |> List.filter ~fn:(fun part -> not (part = ""))
  in
  match parts with
  | [ scheme; encoded ] when String.lowercase_ascii scheme = "basic" -> (
      match decode_basic_payload encoded with
      | Result.Ok decoded -> (
          (* Split on first colon only - password can contain colons *)
          match String.index_of decoded ~char:':' with
          | Option.Some idx ->
              let username = String.sub decoded ~offset:0 ~len:idx in
              let password =
                String.sub decoded ~offset:(idx + 1) ~len:(String.length decoded - idx - 1)
              in
              Ok (username, password)
          | Option.None -> Error MissingCredentialSeparator
        )
      | Result.Error error -> Error error
    )
  | _ -> Error InvalidAuthorizationFormat

let get_credentials = fun conn ->
  let headers = Conn.headers conn in
  match Net.Http.Header.get headers "authorization" with
  | Option.Some auth -> decode_credentials auth
  | Option.None -> Error InvalidAuthorizationFormat

type 'a key = 'a Conn.assign_key

let key = Conn.assign_key

let assign = fun key value conn ->
  Conn.assign key value conn;
  conn

let get = fun key conn -> Conn.get_assign key conn

(** {1 HTTP Responses} *)

(** Send 401 Unauthorized with WWW-Authenticate header *)
let unauthorized = fun conn realm ->
  let sanitized_realm = sanitize_realm realm in
  let auth_header = "Basic realm=\"" ^ sanitized_realm ^ "\"" in
  Conn.respond conn ~status:Unauthorized ~body:"Unauthorized"
  |> Conn.with_header "www-authenticate" auth_header
  |> Conn.halt

(** {1 Middleware} *)

type 'a validation_fn = username:string -> password:string -> 'a option

let middleware = fun ?(realm = "Restricted Area") ?skip ~username ~password () ->
  fun ~conn ~next ->
    (* Check skip condition *)
    let should_skip =
      match skip with
      | Option.Some f -> f conn
      | Option.None -> false
    in
    if should_skip then
      next conn
    else
      (* Extract credentials from Authorization header *)
      match get_credentials conn with
      | Ok (req_user, req_pass) ->
          (* Constant-time comparison for both username and password *)
          let user_match = secure_equal req_user username in
          let pass_match = secure_equal req_pass password in
          if user_match && pass_match then
            next conn
          else
            (* Invalid credentials - return 401 *)
            unauthorized conn realm
      | Error _ ->
          (* No credentials provided - return 401 *)
          unauthorized conn realm

let middleware_with_validation = fun ?(realm = "Restricted Area") ?skip ?assign_to ~validate () ->
  fun ~conn ~next ->
    let should_skip =
      match skip with
      | Option.Some f -> f conn
      | Option.None -> false
    in
    if should_skip then
      next conn
    else
      match get_credentials conn with
      | Ok (username, password) -> (
          match validate ~username ~password with
          | Option.Some user_data ->
              let conn' =
                match assign_to with
                | Some key -> assign key user_data conn
                | None -> conn
              in
              next conn'
          | Option.None -> unauthorized conn realm
        )
      | Error _ -> unauthorized conn realm
