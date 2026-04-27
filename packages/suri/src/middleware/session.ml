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

type decode_error =
  | InvalidCookieFormat of { parts: int }
  | InvalidSignature
  | InvalidPayloadBase64
  | InvalidJson of Data.Json.error
  | InvalidSessionData of Data.Json.t

let secret_error_to_string = function
  | Missing -> "session secret must not be empty"
  | TooShort len -> "session secret must be at least 32 characters long, got " ^ Int.to_string len

let decode_error_to_string = function
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

let require_valid_secret = fun secret ->
  match validate_secret secret with
  | Ok () -> ()
  | Error error -> panic (secret_error_to_string error)

(** Extend Conn.assign_value to store sessions *)
type Conn.assign_value +=
  | Session_data of t

(** Create empty session *)
let create = fun ~cookie_name ~secret () ->
  require_valid_secret secret;
  let now =
    Time.SystemTime.secs (Time.SystemTime.now ())
    |> Int64.of_int
  in
  let data = { values = HashMap.create (); created_at = now; expires_at = Option.none } in
  {
    data;
    cookie_name;
    secret;
    modified = false;
  }

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
        |> Int64.of_int
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
            | Option.Some n -> Int64.of_int n
            | Option.None ->
                Time.SystemTime.secs (Time.SystemTime.now ())
                |> Int64.of_int
          )
        | Option.None ->
            Time.SystemTime.secs (Time.SystemTime.now ())
            |> Int64.of_int
      in
      let expires_at =
        match get_field "expires_at" json with
        | Option.Some Null -> Option.none
        | Option.Some v -> (
            match get_int v with
            | Option.Some n -> Option.some (Int64.of_int n)
            | Option.None -> Option.none
          )
        | Option.None -> Option.none
      in
      Option.some { values; created_at; expires_at }
  | _ -> Option.none

(** {1 Cookie Protection} *)

(** Simple XOR encryption - placeholder for real AES-GCM *)
let encrypt = fun ~secret data ->
  let secret_len = String.length secret in
  let data_len = String.length data in
  let bytes = IO.Bytes.create ~size:data_len in
  for i = data_len - 1 downto 0 do
    let secret_byte = String.get_unchecked secret ~at:(i mod secret_len) in
    let data_byte = String.get_unchecked data ~at:i in
    let encrypted_byte = Char.chr (Char.code secret_byte lxor Char.code data_byte) in
    IO.Bytes.set_unchecked bytes ~at:i ~char:encrypted_byte
  done;
  String.from_bytes bytes

(** Simple XOR decryption - same as encryption for XOR *)
let decrypt = fun ~secret encrypted -> encrypt ~secret encrypted

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

let digest_to_string = fun digest ->
  digest
  |> Crypto.Digest.bytes
  |> IO.Bytes.to_string

let normalize_hmac_key = fun secret ->
  let key =
    if String.length secret > 64 then
      Crypto.Sha256.hash_string secret
      |> digest_to_string
    else
      secret
  in
  if String.length key < 64 then
    key ^ String.make ~len:(64 - String.length key) ~char:'\000'
  else
    key

let xor_with_byte = fun data byte ->
  String.init
    ~len:(String.length data)
    ~fn:(fun index ->
      let value = Char.code (String.get_unchecked data ~at:index) lxor byte in
      Char.chr value)

let hmac_sha256 = fun ~secret data ->
  let key = normalize_hmac_key secret in
  let inner_key = xor_with_byte key 0x36 in
  let outer_key = xor_with_byte key 0x5c in
  let inner_hash =
    Crypto.Sha256.hash_string (inner_key ^ data)
    |> digest_to_string
  in
  Crypto.Sha256.hash_string (outer_key ^ inner_hash)
  |> digest_to_string

(** HMAC-SHA256 signature for cookie integrity. *)
let sign = fun ~secret data ->
  hmac_sha256 ~secret data
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
  let encrypted = encrypt ~secret:session.secret json_str in
  let encrypted_b64 = Encoding.Base64.encode encrypted in
  let signature = sign ~secret:session.secret encrypted_b64 in
  String.concat "." [ encrypted_b64; signature ]

let cookie_value_for_plaintext = fun ~secret plaintext ->
  let encrypted = encrypt ~secret plaintext in
  let encrypted_b64 = Encoding.Base64.encode encrypted in
  let signature = sign ~secret encrypted_b64 in
  String.concat "." [ encrypted_b64; signature ]

(** Deserialize session from cookie value *)
let from_cookie_value = fun ~cookie_name ~secret cookie_value ->
  let parts = String.split_on_char '.' cookie_value in
  match parts with
  | [ encrypted_b64; signature ] ->
      (* Verify signature *)
      if not (verify ~secret encrypted_b64 signature) then
        Error InvalidSignature
      else
        (* Decrypt *)
        (
          match Encoding.Base64.decode encrypted_b64 with
          | Result.Ok encrypted ->
              let json_str = decrypt ~secret encrypted in
              (* Parse JSON *)
              (
                match Data.Json.of_string json_str with
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
          | Result.Error _ -> Error InvalidPayloadBase64
        )
  | _ -> Error (InvalidCookieFormat { parts = List.length parts })

module For_testing = struct
  type nonrec secret_error = secret_error =
    | Missing
    | TooShort of int

  type nonrec decode_error = decode_error =
    | InvalidCookieFormat of { parts: int }
    | InvalidSignature
    | InvalidPayloadBase64
    | InvalidJson of Data.Json.error
    | InvalidSessionData of Data.Json.t

  let create = create

  let validate_secret = validate_secret

  let secret_error_to_string = secret_error_to_string

  let decode_error_to_string = decode_error_to_string

  let sign = sign

  let verify = verify

  let to_cookie_value = to_cookie_value

  let cookie_value_for_plaintext = cookie_value_for_plaintext

  let from_cookie_value = from_cookie_value
end

(** {1 Middleware} *)

(** Storage key for session in connection *)
let session_key = "suri.session"

(** Find session from connection *)
let find = fun conn ->
  match Conn.get_assign session_key conn with
  | Option.Some (Session_data session) -> Option.some session
  | _ -> Option.none

(** Get session from connection - creates new if not present *)
let get = fun conn ->
  match find conn with
  | Option.Some session -> session
  | Option.None ->
      panic "Suri.Middleware.Session.get called before Session.middleware installed a session"

(** Session middleware *)
let middleware = fun
  ~secret
  ?(cookie_name = "_suri_session")
  ?(max_age = 86_400)
  ?(secure = false)
  ?(same_site = Http.Http1.Cookie.Lax)
  () ->
  require_valid_secret secret;
  fun ~conn ~next ->
    (* Try to load session from cookie *)
    let headers = Conn.headers conn in
    let cookie_header = Net.Http.Header.get headers "cookie" in
    let session =
      match cookie_header with
      | Option.None -> create ~cookie_name ~secret ()
      | Option.Some header ->
          let cookies = Http.Http1.Cookie.parse header in
          (
            match Std.Collections.Proplist.get cookies ~key:cookie_name with
            | Option.None -> create ~cookie_name ~secret ()
            | Option.Some cookie_value -> (
                match from_cookie_value ~cookie_name ~secret cookie_value with
                | Result.Ok sess ->
                    if is_expired sess then
                      create ~cookie_name ~secret ()
                    else
                      sess
                | Result.Error _err ->
                    (* Invalid cookie - create new session *)
                    create ~cookie_name ~secret ()
              )
          )
    in
    (* Store session in connection *)
    Conn.assign session_key (Session_data session) conn;
    (* Call next handler *)
    let conn' = next conn in
    (* If session was modified, set cookie in response *)
    if is_modified session then
      begin
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
        let set_cookie_header = Http.Http1.Cookie.to_set_cookie cookie in
        Conn.with_header "set-cookie" set_cookie_header conn'
      end
    else
      conn'
