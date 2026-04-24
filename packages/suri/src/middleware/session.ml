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

(** Extend Conn.assign_value to store sessions *)
type Conn.assign_value +=
  Session_data of t

(** Create empty session *)
let create = fun ~cookie_name ~secret () ->
  let now = Time.SystemTime.secs (Time.SystemTime.now ()) |> Int64.of_int in
  let data = { values = HashMap.create (); created_at = now; expires_at = Option.none } in
  { data; cookie_name; secret; modified = false }

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
      let now = Time.SystemTime.secs (Time.SystemTime.now ()) |> Int64.of_int in
      Int64.compare now exp > 0
  | Option.None -> false

(** Check if session was modified *)
let is_modified = fun session -> session.modified

(** {1 JSON Serialization} *)

(** Serialize session data to JSON *)
let to_json = fun data ->
  let open Data.Json in
    let values_list = HashMap.to_list data.values |> List.map ~fn:(fun ((k, v)) -> (k, string v)) in
    obj
      [ ("values", obj values_list); ("created_at", int (Int64.to_int data.created_at)); (
          "expires_at",
          match data.expires_at with
          | Option.Some exp -> int (Int64.to_int exp)
          | Option.None -> null
        ); ]

(** Deserialize session data from JSON *)
let from_json = fun json ->
  let open Data.Json in
    match json with
    | Object _ ->
        let values =
          match get_field "values" json with
          | Option.Some (Object pairs) ->
              let hm = HashMap.create () in
              List.for_each pairs
                ~fn:(fun ((k, v)) ->
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
              | Option.None -> Time.SystemTime.secs (Time.SystemTime.now ()) |> Int64.of_int
            )
          | Option.None -> Time.SystemTime.secs (Time.SystemTime.now ()) |> Int64.of_int
        in
        let expires_at =
          match get_field "expires_at" json with
          | Option.Some (Null) ->
              Option.none
          | Option.Some v -> (
              match get_int v with
              | Option.Some n -> Option.some (Int64.of_int n)
              | Option.None -> Option.none
            )
          | Option.None ->
              Option.none
        in
        Option.some { values; created_at; expires_at }
    | _ -> Option.none

(** {1 Basic Crypto (XOR placeholder - NOT SECURE for production)} *)

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

(** Simple signature using hash of combined secret and data *)
let sign = fun ~secret data ->
  let combined = String.concat ":" [ secret; data ] in
  (* Simple hash: sum of all byte values *)
  let sum = ref 0 in
  for i = 0 to String.length combined - 1 do
    sum := !sum + Char.code (String.get_unchecked combined ~at:i)
  done;
  (* Convert to hex string *)
  String.concat "" [ "0x"; Int.to_string !sum ]

(** Verify signature *)
let verify = fun ~secret data signature ->
  let expected = sign ~secret data in
  String.equal expected signature

(** {1 Cookie Serialization} *)

(** Serialize session to cookie value *)
let to_cookie_value = fun session ->
  let json = to_json session.data in
  let json_str = Data.Json.to_string json in
  (* Encrypt *)
  let encrypted = encrypt ~secret:session.secret json_str in
  let encrypted_b64 = Encoding.Base64.encode encrypted in
  (* Sign *)
  let signature = sign ~secret:session.secret encrypted_b64 in
  (* Return: encrypted.signature *)
  String.concat "." [ encrypted_b64; signature ]

(** Deserialize session from cookie value *)
let from_cookie_value = fun ~cookie_name ~secret cookie_value ->
  match String.split_on_char '.' cookie_value with
  | [encrypted_b64;signature] ->
      (* Verify signature *)
      if not (verify ~secret encrypted_b64 signature) then
        Result.err "Invalid signature"
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
                    | Option.Some data -> Result.ok { data; cookie_name; secret; modified = false }
                    | Option.None -> Result.err "Invalid session data"
                  )
                | Result.Error _err -> Result.err "Invalid JSON in session"
              )
          | Result.Error _ -> Result.err "Invalid base64 encoding"
        )
  | _ -> Result.err "Invalid cookie format"

(** {1 Middleware} *)

(** Storage key for session in connection *)
let session_key = "suri.session"

(** Get session from connection - creates new if not present *)
let get = fun conn ->
  match Conn.get_assign session_key conn with
  | Option.Some (Session_data session) -> session
  | _ ->
      (* This shouldn't happen if middleware is installed *)
      create ~cookie_name:"_suri_session" ~secret:"" ()

(** Session middleware *)
let middleware = fun ~secret ?(cookie_name = "_suri_session") ?(max_age = 86_400) ?(secure = false) ?(same_site = Http.Http1.Cookie.Lax) () ->
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
        let cookie = Http.Http1.Cookie.make
          ~name:cookie_name
          ~value:cookie_value
          ~max_age
          ~path:"/"
          ~secure
          ~http_only:true
          ~same_site
          () in
        let set_cookie_header = Http.Http1.Cookie.to_set_cookie cookie in
        Conn.with_header "set-cookie" set_cookie_header conn'
      end
    else
      conn'
