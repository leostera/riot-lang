open Std
open Std.Collections

(** Session data stored in cookie *)
type session_data = {
  values: (string, string) HashMap.t;
  created_at: int64;
  mutable expires_at: int64 option;
}

(** Session handle *)
type t = {
  data: session_data;
  cookie_name: string;
  secret: string;
  mutable modified: bool;
}

type secret_error =
  | Missing
  | TooShort of int

type cookie_name_error =
  | EmptyCookieName
  | InvalidCookieNameChar of { char: char; index: int }

type setup_error =
  | InvalidSecret of secret_error
  | InvalidCookieName of cookie_name_error
  | InvalidMaxAge of int
  | SameSiteNoneRequiresSecure

type decode_error =
  | InvalidCookieFormat of { parts: int }
  | InvalidSignature
  | InvalidPayloadBase64
  | InvalidJson of Data.Json.error
  | InvalidSessionData of Data.Json.t

let secret_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Missing -> "session secret must not be empty"
  | TooShort len -> "session secret must be at least 32 characters long, got " ^ Int.to_string len

let cookie_name_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | EmptyCookieName -> "session cookie name must not be empty"
  | InvalidCookieNameChar { char; index } ->
      "session cookie name contains invalid character code "
      ^ Int.to_string (Char.code char)
      ^ " at index "
      ^ Int.to_string index

let setup_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidSecret error -> secret_error_to_string error
  | InvalidCookieName error -> cookie_name_error_to_string error
  | InvalidMaxAge value -> "session max_age must be greater than 0, got " ^ Int.to_string value
  | SameSiteNoneRequiresSecure -> "session cookies with SameSite=None must also set Secure"

let decode_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidCookieFormat { parts } ->
      "invalid cookie format; expected '<payload>.<signature>', got "
      ^ Int.to_string parts
      ^ " parts"
  | InvalidSignature -> "invalid signature"
  | InvalidPayloadBase64 -> "invalid base64 encoding in payload"
  | InvalidJson error -> "invalid JSON in session: " ^ Data.Json.error_to_string error
  | InvalidSessionData json -> "invalid session data: " ^ Data.Json.to_string json

let validate_secret = fun secret ->
  let trimmed = String.trim secret in
  if String.equal trimmed "" then
    Error Missing
  else if String.length secret < 32 then
    Error (TooShort (String.length secret))
  else
    Ok ()

let is_cookie_name_char = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '-' -> true
  | _ -> false

let validate_cookie_name = fun cookie_name ->
  let len = String.length cookie_name in
  if len = 0 then
    Error EmptyCookieName
  else
    let rec go index =
      if index >= len then
        Ok ()
      else
        let char = String.get_unchecked cookie_name ~at:index in
        if is_cookie_name_char char then
          go (index + 1)
        else
          Error (InvalidCookieNameChar { char; index })
    in
    go 0

let validate_setup = fun ~secret ~cookie_name ~max_age ~secure ~same_site ->
  match validate_secret secret with
  | Error error -> Error (InvalidSecret error)
  | Ok () -> (
      match validate_cookie_name cookie_name with
      | Error error -> Error (InvalidCookieName error)
      | Ok () ->
          if max_age <= 0 then
            Error (InvalidMaxAge max_age)
          else
            match (same_site, secure) with
            | (Http.Http1.Cookie.None, false) -> Error SameSiteNoneRequiresSecure
            | (Http.Http1.Cookie.Strict, _)
            | (Http.Http1.Cookie.Lax, _)
            | (Http.Http1.Cookie.None, true) -> Ok ()
    )

let session_key: t Conn.assign_key = Conn.assign_key ()

(** Create empty session *)
let create_validated = fun ~cookie_name ~secret () ->
  let now =
    Time.SystemTime.secs (Time.SystemTime.now ())
    |> Int64.from_int
  in
  let data = { values = HashMap.create (); created_at = now; expires_at = Option.none } in
  {
    data;
    cookie_name;
    secret;
    modified = false;
  }

let create = fun ~cookie_name ~secret () ->
  match validate_secret secret with
  | Error error -> Error (InvalidSecret error)
  | Ok () -> (
      match validate_cookie_name cookie_name with
      | Error error -> Error (InvalidCookieName error)
      | Ok () -> Ok (create_validated ~cookie_name ~secret ())
    )

(** Get value from session *)
let get_value = fun key session -> HashMap.get session.data.values ~key

(** Put value in session *)
let put = fun key value session ->
  let _ = HashMap.insert session.data.values ~key ~value in
  session.modified <- true

(** Delete value from session *)
let delete = fun key session ->
  let _ = HashMap.remove session.data.values ~key in
  session.modified <- true

(** Clear all session data *)
let clear = fun session ->
  HashMap.clear session.data.values;
  session.modified <- true

(** Check if session is expired *)
let is_expired = fun session ->
  match session.data.expires_at with
  | Option.Some exp ->
      let now =
        Time.SystemTime.secs (Time.SystemTime.now ())
        |> Int64.from_int
      in
      Int64.compare now exp = Order.GT
  | Option.None -> false

(** Check if session was modified *)
let is_modified = fun session -> session.modified

(** {1 JSON Serialization} *)

(** Serialize session data to JSON *)
let to_json = fun data ->
  let open Data.Json in
  let values_list =
    HashMap.to_list data.values
    |> List.map ~fn:(fun (k, v) -> (k, string v))
  in
  obj
    [
      ("values", obj values_list);
      ("created_at", int (Int64.to_int data.created_at));
      ("expires_at", match data.expires_at with
      | Option.Some exp -> int (Int64.to_int exp)
      | Option.None -> null);
    ]

(** Deserialize session data from JSON *)
let from_json = fun json ->
  let open Data.Json in
  match json with
  | Object _ ->
      let values =
        match get_field "values" json with
        | Option.Some (Object pairs) ->
            let hm = HashMap.create () in
            List.for_each
              pairs
              ~fn:(fun (k, v) ->
                match get_string v with
                | Option.Some s ->
                    let _ = HashMap.insert hm ~key:k ~value:s in
                    ()
                | Option.None -> ());
            hm
        | _ -> HashMap.create ()
      in
      let created_at =
        match get_field "created_at" json with
        | Option.Some v -> (
            match get_int v with
            | Option.Some n -> Int64.from_int n
            | Option.None ->
                Time.SystemTime.secs (Time.SystemTime.now ())
                |> Int64.from_int
          )
        | Option.None ->
            Time.SystemTime.secs (Time.SystemTime.now ())
            |> Int64.from_int
      in
      let expires_at =
        match get_field "expires_at" json with
        | Option.Some Null -> Option.none
        | Option.Some v -> (
            match get_int v with
            | Option.Some n -> Option.some (Int64.from_int n)
            | Option.None -> Option.none
          )
        | Option.None -> Option.none
      in
      Option.some { values; created_at; expires_at }
  | _ -> Option.none

(** {1 Cookie Protection} *)

let secure_equal = fun s1 s2 ->
  if not (String.length s1 = String.length s2) then
    false
  else
    let mismatch = ref 0 in
    for i = 0 to String.length s1 - 1 do
      let c1 = Char.code (String.get_unchecked s1 ~at:i) in
      let c2 = Char.code (String.get_unchecked s2 ~at:i) in
      mismatch := !mismatch lor (c1 lxor c2)
    done;
  !mismatch = 0

(** HMAC-SHA256 signature for cookie integrity. *)
let sign = fun ~secret data ->
  Crypto.hmac_sha256 ~key:secret ~data
  |> Encoding.Base64.encode

(** Verify signature *)
let verify = fun ~secret data signature ->
  let expected = sign ~secret data in
  secure_equal expected signature

(** {1 Cookie Serialization} *)

(** Serialize session to cookie value *)
let to_cookie_value = fun session ->
  let json = to_json session.data in
  let json_str = Data.Json.to_string json in
  let payload = Encoding.Base64.encode json_str in
  let signature = sign ~secret:session.secret payload in
  String.concat "." [ payload; signature ]

let cookie_value_for_plaintext = fun ~secret plaintext ->
  let payload = Encoding.Base64.encode plaintext in
  let signature = sign ~secret payload in
  String.concat "." [ payload; signature ]

let decode_payload = fun payload ->
  match Encoding.Base64.decode payload with
  | Result.Ok decoded -> Ok decoded
  | Result.Error Encoding.Base64.InvalidBase64 -> Error InvalidPayloadBase64

(** Deserialize session from cookie value *)
let from_cookie_value = fun ~cookie_name ~secret cookie_value ->
  let parts = String.split_on_char '.' cookie_value in
  match parts with
  | [ payload; signature ] ->
      (* Verify signature *)
      if not (verify ~secret payload signature) then
        Error InvalidSignature
      else
        (
          match decode_payload payload with
          | Result.Ok json_str -> (
              match Data.Json.from_string json_str with
              | Result.Ok json -> (
                  match from_json json with
                  | Option.Some data ->
                      Ok {
                        data;
                        cookie_name;
                        secret;
                        modified = false;
                      }
                  | Option.None -> Error (InvalidSessionData json)
                )
              | Result.Error err -> Error (InvalidJson err)
            )
          | Result.Error error -> Error error
        )
  | _ -> Error (InvalidCookieFormat { parts = List.length parts })

(** {1 Middleware} *)

(** Find session from connection *)
let find = fun conn -> Conn.get_assign session_key conn

(** Get session from connection. *)
let get = find

(** Session middleware *)
let middleware = fun
  ~secret
  ?(cookie_name = "_suri_session")
  ?(max_age = 86_400)
  ?(secure = false)
  ?(same_site = Http.Http1.Cookie.Lax)
  () ->
  match validate_setup ~secret ~cookie_name ~max_age ~secure ~same_site with
  | Error error -> Error error
  | Ok () ->
      Ok (fun ~conn ~next ->
        (* Try to load session from cookie *)
        let headers = Conn.headers conn in
        let cookie_header = Net.Http.Header.get headers "cookie" in
        let session =
          match cookie_header with
          | Option.None -> create_validated ~cookie_name ~secret ()
          | Option.Some header ->
              let cookies = Http.Http1.Cookie.parse header in
              (
                match Std.Collections.Proplist.get cookies ~key:cookie_name with
                | Option.None -> create_validated ~cookie_name ~secret ()
                | Option.Some cookie_value -> (
                    match from_cookie_value ~cookie_name ~secret cookie_value with
                    | Result.Ok sess ->
                        if is_expired sess then
                          create_validated ~cookie_name ~secret ()
                        else
                          sess
                    | Result.Error _err ->
                        (* Invalid cookie - create new session *)
                        create_validated ~cookie_name ~secret ()
                  )
              )
        in
        (* Store session in connection *)
        let conn = Conn.assign session_key session conn in
        (* Call next handler *)
        let conn' = next conn in
        (* If session was modified, set cookie in response *)
        if is_modified session then (
          let cookie_value = to_cookie_value session in
          let cookie =
            Http.Http1.Cookie.make
              ~name:cookie_name
              ~value:cookie_value
              ~max_age
              ~path:"/"
              ~secure
              ~http_only:true
              ~same_site
              ()
          in
          match cookie with
          | Ok cookie ->
              let set_cookie_header = Http.Http1.Cookie.to_set_cookie cookie in
              Conn.with_header "set-cookie" set_cookie_header conn'
          | Error error ->
              conn'
              |> Conn.respond
                ~status:Net.Http.Status.InternalServerError
                ~body:(Http.Http1.Cookie.validation_error_to_string error)
              |> Conn.send
        ) else
          conn')
