open Std
open Std.IO
open Result.Syntax

module Config = Postgres_config
module Ser = Serde.Ser

external hmac_sha256_bytes: string -> string -> bytes = "std_crypto_hmac_sha256"

type config = Config.t

(* Proper error type that distinguishes transport vs protocol errors *)

type error =
  | AddressError of Net.Addr.error
  | TransportError of Net.TcpStream.error
  | TransportIoError of IO.error
  | ProtocolError of Protocol.Error.t
  | ConnectionClosed
  | AuthenticationNotSupported of authentication_error
  | TlsNotSupported of tls_error
  | TlsError of Net.TlsStream.error
  | UnexpectedMessage of unexpected_message

and authentication_error =
  | UnsupportedSaslMechanisms of string list

and tls_error =
  | ServerRejectedTls

and unexpected_message =
  | ScramServerFirstInvalidIterationCount
  | ScramServerFirstInvalidSalt
  | ScramServerFirstMissingFields
  | ScramServerSignatureMismatch
  | ScramServerFinalMissingVerifier
  | InvalidBackendMessageLength of int
  | BackendMessageBodyTooLarge of { length: int; limit: int }
  | InvalidBackendMessage of Protocol.Reader.parse_error
  | ExpectedScramFinalMessage
  | ExpectedScramContinueMessage
  | ScramContinueWithoutSaslStart
  | ScramFinalWithoutSaslStart
  | HandshakeUnexpectedMessageType of int
  | QueryUnexpectedMessageType of int
  | TlsUnexpectedResponse of int
  | TransactionAlreadyInProgress
  | NoTransactionInProgress

type error_document = {
  type_: string;
  error: string option;
  kind: string option;
  message: string;
  mode: string option;
  length: int option;
  limit: int option;
  message_type: int option;
  offset: int option;
}

type connection = {
  id: string;
  stream: transport option;
  config: config;
  mutable statement_seq: int;
  mutable transaction_status: char;
  mutable closed: bool;
}

and transport = {
  tcp: Net.TcpStream.t;
  mutable tls: Net.TcpStream.t Net.TlsStream.t option;
  mutable reader: IO.Reader.t;
  mutable writer: IO.Writer.t;
}

and statement = {
  name: string;
  sql: string;
  conn: connection;
}

type result_set = {
  rows: Sqlx_driver.Row.t Collections.Queue.t;
  mutable rows_affected: int;
}

let name = "PostgreSQL"

let addr_error_to_string = fun error ->
  match error with
  | Net.Addr.System_error io_err -> IO.error_message io_err
  | Net.Addr.Invalid_port_number value -> "invalid port number: " ^ value
  | Net.Addr.Invalid_format value -> "invalid address format: " ^ value

let authentication_error_to_string = fun error ->
  match error with
  | UnsupportedSaslMechanisms mechanisms -> "SASL mechanisms: " ^ String.concat "," mechanisms

let tls_error_to_string = fun error ->
  match error with
  | ServerRejectedTls -> "server rejected TLS"

let tls_stream_error_to_string = fun error ->
  match error with
  | Net.TlsStream.Closed -> "TLS stream closed"
  | Net.TlsStream.Handshake_failed message -> "TLS handshake failed: " ^ message
  | Net.TlsStream.System_error error -> IO.error_message error
  | Net.TlsStream.Network_read_failed error -> "TLS network read failed: " ^ IO.error_message error
  | Net.TlsStream.Network_write_failed error ->
      "TLS network write failed: " ^ IO.error_message error
  | Net.TlsStream.Tls_not_available -> "TLS is not available"
  | Net.TlsStream.Unsupported_vectored_operation -> "TLS vectored operation is not supported"

let unexpected_message_to_string = fun error ->
  match error with
  | ScramServerFirstInvalidIterationCount -> "SCRAM server-first-message has invalid iteration count"
  | ScramServerFirstInvalidSalt -> "SCRAM server-first-message has invalid salt"
  | ScramServerFirstMissingFields -> "SCRAM server-first-message is missing required fields"
  | ScramServerSignatureMismatch -> "SCRAM server signature mismatch"
  | ScramServerFinalMissingVerifier -> "SCRAM server-final-message is missing verifier"
  | InvalidBackendMessageLength length ->
      "Invalid PostgreSQL backend message length: " ^ Int.to_string length
  | BackendMessageBodyTooLarge { length; limit } ->
      "PostgreSQL backend message exceeds maximum supported body length: "
      ^ Int.to_string length
      ^ " (limit "
      ^ Int.to_string limit
      ^ ")"
  | InvalidBackendMessage error ->
      "Invalid PostgreSQL backend message: " ^ Protocol.Reader.parse_error_to_string error
  | ExpectedScramFinalMessage -> "Expected SCRAM final message"
  | ExpectedScramContinueMessage -> "Expected SCRAM continue message"
  | ScramContinueWithoutSaslStart -> "SCRAM continue without SASL start"
  | ScramFinalWithoutSaslStart -> "SCRAM final without SASL start"
  | HandshakeUnexpectedMessageType message_type ->
      "During handshake: " ^ String.make ~len:1 ~char:(Char.from_int_unchecked message_type)
  | QueryUnexpectedMessageType message_type ->
      "During query: " ^ String.make ~len:1 ~char:(Char.from_int_unchecked message_type)
  | TlsUnexpectedResponse message_type ->
      "During TLS negotiation: " ^ String.make ~len:1 ~char:(Char.from_int_unchecked message_type)
  | TransactionAlreadyInProgress -> "Transaction already in progress"
  | NoTransactionInProgress -> "No transaction in progress"

let unexpected_message_kind = fun error ->
  match error with
  | ScramServerFirstInvalidIterationCount -> "scram_server_first_invalid_iteration_count"
  | ScramServerFirstInvalidSalt -> "scram_server_first_invalid_salt"
  | ScramServerFirstMissingFields -> "scram_server_first_missing_fields"
  | ScramServerSignatureMismatch -> "scram_server_signature_mismatch"
  | ScramServerFinalMissingVerifier -> "scram_server_final_missing_verifier"
  | InvalidBackendMessageLength _ -> "invalid_backend_message_length"
  | BackendMessageBodyTooLarge _ -> "backend_message_body_too_large"
  | InvalidBackendMessage _ -> "invalid_backend_message"
  | ExpectedScramFinalMessage -> "expected_scram_final_message"
  | ExpectedScramContinueMessage -> "expected_scram_continue_message"
  | ScramContinueWithoutSaslStart -> "scram_continue_without_sasl_start"
  | ScramFinalWithoutSaslStart -> "scram_final_without_sasl_start"
  | HandshakeUnexpectedMessageType _ -> "handshake_unexpected_message_type"
  | QueryUnexpectedMessageType _ -> "query_unexpected_message_type"
  | TlsUnexpectedResponse _ -> "tls_unexpected_response"
  | TransactionAlreadyInProgress -> "transaction_already_in_progress"
  | NoTransactionInProgress -> "no_transaction_in_progress"

let error_to_string = fun error ->
  match error with
  | AddressError addr_error -> "Address error: " ^ addr_error_to_string addr_error
  | TransportError Net.TcpStream.Connection_refused -> "Connection refused"
  | TransportError Net.TcpStream.Closed -> "Connection closed"
  | TransportError (Net.TcpStream.System_error io_err) ->
      "Transport error: " ^ IO.error_message io_err
  | TransportIoError io_err -> "Transport error: " ^ IO.error_message io_err
  | ProtocolError proto_err -> Protocol.Error.to_string proto_err
  | ConnectionClosed -> "Connection is closed"
  | AuthenticationNotSupported error ->
      "Authentication method not supported: " ^ authentication_error_to_string error
  | TlsNotSupported error -> "PostgreSQL TLS negotiation failed: " ^ tls_error_to_string error
  | TlsError error -> "PostgreSQL TLS error: " ^ tls_stream_error_to_string error
  | UnexpectedMessage error -> "Unexpected message: " ^ unexpected_message_to_string error

let error_document = fun error ->
  match error with
  | AddressError addr_error ->
      let (error, message) =
        match addr_error with
        | Net.Addr.System_error io_err -> ("system_error", IO.error_message io_err)
        | Net.Addr.Invalid_port_number value ->
            ("invalid_port_number", "invalid port number: " ^ value)
        | Net.Addr.Invalid_format value -> ("invalid_format", "invalid address format: " ^ value)
      in
      {
        type_ = "address_error";
        error = Some error;
        kind = None;
        message;
        mode = None;
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
  | TransportError Net.TcpStream.Connection_refused ->
      {
        type_ = "transport_error";
        error = Some "connection_refused";
        kind = None;
        message = "Connection refused";
        mode = None;
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
  | TransportError Net.TcpStream.Closed ->
      {
        type_ = "transport_error";
        error = Some "closed";
        kind = None;
        message = "Connection closed";
        mode = None;
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
  | TransportError (Net.TcpStream.System_error io_err) ->
      {
        type_ = "transport_error";
        error = Some "system_error";
        kind = None;
        message = IO.error_message io_err;
        mode = None;
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
  | TransportIoError io_err ->
      {
        type_ = "transport_error";
        error = Some "io_error";
        kind = None;
        message = IO.error_message io_err;
        mode = None;
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
  | ProtocolError proto_err ->
      {
        type_ = "protocol_error";
        error = None;
        kind = None;
        message = Protocol.Error.to_string proto_err;
        mode = None;
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
  | ConnectionClosed ->
      {
        type_ = "connection_closed";
        error = None;
        kind = None;
        message = "Connection is closed";
        mode = None;
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
  | AuthenticationNotSupported auth_error ->
      {
        type_ = "authentication_not_supported";
        error = None;
        kind = None;
        message = "Authentication method not supported: "
        ^ authentication_error_to_string auth_error;
        mode = None;
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
  | TlsNotSupported tls_error ->
      {
        type_ = "tls_not_supported";
        error = None;
        kind = None;
        message = "PostgreSQL TLS negotiation failed: " ^ tls_error_to_string tls_error;
        mode = Some (tls_error_to_string tls_error);
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
  | TlsError tls_error ->
      {
        type_ = "tls_error";
        error = None;
        kind = None;
        message = tls_stream_error_to_string tls_error;
        mode = None;
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
  | UnexpectedMessage unexpected ->
      let document = {
        type_ = "unexpected_message";
        error = None;
        kind = Some (unexpected_message_kind unexpected);
        message = unexpected_message_to_string unexpected;
        mode = None;
        length = None;
        limit = None;
        message_type = None;
        offset = None;
      }
      in
      (
        match unexpected with
        | InvalidBackendMessageLength length -> { document with length = Some length }
        | BackendMessageBodyTooLarge { length; limit } ->
            { document with length = Some length; limit = Some limit }
        | HandshakeUnexpectedMessageType message_type
        | QueryUnexpectedMessageType message_type
        | TlsUnexpectedResponse message_type ->
            { document with message_type = Some message_type }
        | InvalidBackendMessage error ->
            {
              document with
              message_type = Some error.message_type;
              length = Some error.length;
              offset = Some error.offset;
            }
        | _ -> document
      )

let error_serializer =
  Ser.contramap
    error_document
    (
      Ser.record
        (
          Ser.fields
            [
              Ser.field "type" Ser.string (fun (error: error_document) -> error.type_);
              Ser.field "error" (Ser.option Ser.string) (fun error -> error.error);
              Ser.field "kind" (Ser.option Ser.string) (fun error -> error.kind);
              Ser.field "message" Ser.string (fun error -> error.message);
              Ser.field "mode" (Ser.option Ser.string) (fun error -> error.mode);
              Ser.field "length" (Ser.option Ser.int) (fun error -> error.length);
              Ser.field "limit" (Ser.option Ser.int) (fun error -> error.limit);
              Ser.field "message_type" (Ser.option Ser.int) (fun error -> error.message_type);
              Ser.field "offset" (Ser.option Ser.int) (fun error -> error.offset);
            ]
        )
    )

let transport_of_tcp = fun tcp ->
  {
    tcp;
    tls = None;
    reader = Net.TcpStream.to_reader tcp;
    writer = Net.TcpStream.to_writer tcp;
  }

let close_transport = fun transport ->
  (
    match transport.tls with
    | Some tls -> Net.TlsStream.close tls
    | None -> ()
  );
  Net.TcpStream.close transport.tcp

let write_tcp_all = fun tcp msg ->
  let bytes = Bytes.from_string msg in
  let total = Bytes.length bytes in
  let rec loop offset =
    if offset >= total then
      Ok ()
    else
      match Net.TcpStream.write tcp bytes ~pos:offset ~len:(total - offset) () with
      | Error err -> Error (TransportError err)
      | Ok 0 -> Error (TransportError Net.TcpStream.Closed)
      | Ok written -> loop (offset + written)
  in
  loop 0

let read_tcp_byte = fun tcp ->
  let byte = Bytes.create ~size:1 in
  match Net.TcpStream.read tcp byte ~pos:0 ~len:1 () with
  | Error err -> Error (TransportError err)
  | Ok 0 -> Error (TransportError Net.TcpStream.Closed)
  | Ok _ -> Ok (Bytes.get_unchecked byte ~at:0)

let write_message = fun transport msg ->
  match IO.Writer.write_all transport.writer ~from:(IO.Buffer.from_string msg) with
  | Error error -> Error (TransportIoError error)
  | Ok () -> (
      match IO.Writer.flush transport.writer with
      | Error error -> Error (TransportIoError error)
      | Ok () -> Ok ()
    )

let hmac_sha256 = fun ~key ~data ->
  hmac_sha256_bytes key data
  |> Bytes.to_string

let sha256_bytes = fun data ->
  Crypto.Sha256.hash_string data
  |> Crypto.Digest.bytes
  |> Bytes.to_string

let md5_hex = fun data ->
  Crypto.Md5.hash_string data
  |> Crypto.Digest.hex

let xor_strings = fun left right ->
  let len = String.length left in
  let bytes = Bytes.create ~size:len in
  for index = 0 to len - 1 do
    let l = Char.code (String.get_unchecked left ~at:index) in
    let r = Char.code (String.get_unchecked right ~at:index) in
    Bytes.set_unchecked bytes ~at:index ~char:(Char.from_int_unchecked (l lxor r))
  done;
  Bytes.to_string bytes

let int32_be = fun value ->
  let bytes = Bytes.create ~size:4 in
  Bytes.set_unchecked bytes ~at:0 ~char:(Char.from_int_unchecked ((value lsr 24) land 0xff));
  Bytes.set_unchecked bytes ~at:1 ~char:(Char.from_int_unchecked ((value lsr 16) land 0xff));
  Bytes.set_unchecked bytes ~at:2 ~char:(Char.from_int_unchecked ((value lsr 8) land 0xff));
  Bytes.set_unchecked bytes ~at:3 ~char:(Char.from_int_unchecked (value land 0xff));
  Bytes.to_string bytes

let ssl_request_code = 80_877_103

let negotiate_tls = fun transport (cfg: Config.t) ->
  match cfg.ssl_mode with
  | Config.Disable -> Ok ()
  | Config.Prefer
  | Config.Require -> (
      match write_tcp_all transport.tcp (int32_be 8 ^ int32_be ssl_request_code) with
      | Error _ as error -> error
      | Ok () -> (
          match read_tcp_byte transport.tcp with
          | Error _ as error -> error
          | Ok 'S' -> (
              match Net.TlsStream.from_client_io
                ~reader:transport.reader
                ~writer:transport.writer
                ~hostname:cfg.host
                () with
              | Error error -> Error (TlsError error)
              | Ok tls ->
                  transport.tls <- Some tls;
                  transport.reader <- Net.TlsStream.to_reader tls;
                  transport.writer <- Net.TlsStream.to_writer tls;
                  Ok ()
            )
          | Ok 'N' -> (
              match cfg.ssl_mode with
              | Config.Require -> Error (TlsNotSupported ServerRejectedTls)
              | Config.Prefer -> Ok ()
              | Config.Disable -> Ok ()
            )
          | Ok response -> Error (UnexpectedMessage (TlsUnexpectedResponse (Char.code response)))
        )
    )

let pbkdf2_sha256 = fun ~password ~salt ~iterations ->
  let first = hmac_sha256 ~key:password ~data:(salt ^ int32_be 1) in
  let output = Bytes.from_string first in
  let rec loop remaining previous =
    if remaining <= 1 then
      Bytes.to_string output
    else
      let next = hmac_sha256 ~key:password ~data:previous in
      for index = 0 to String.length next - 1 do
        let current = Char.code (Bytes.get_unchecked output ~at:index) in
        let update = Char.code (String.get_unchecked next ~at:index) in
        Bytes.set_unchecked output ~at:index ~char:(Char.from_int_unchecked (current lxor update))
      done;
    loop (remaining - 1) next
  in
  loop iterations first

let sasl_escape = fun value ->
  let buffer = Buffer.create ~size:(String.length value) in
  String.for_each
    value
    ~fn:(fun char ->
      match char with
      | ',' -> Buffer.add_string buffer "=2C"
      | '=' -> Buffer.add_string buffer "=3D"
      | _ -> Buffer.add_char buffer char);
  Buffer.contents buffer

let scram_nonce = fun () -> "berrybot-" ^ UUID.to_string (UUID.v7_monotonic ())

let has_mechanism = fun mechanisms expected ->
  let rec loop mechanisms =
    match mechanisms with
    | [] -> false
    | mechanism :: rest -> mechanism = expected || loop rest
  in
  loop mechanisms

let field_value = fun key fields ->
  let prefix = key ^ "=" in
  let rec loop fields =
    match fields with
    | [] -> None
    | field :: rest ->
        if String.starts_with ~prefix field then
          Some (String.sub
            field
            ~offset:(String.length prefix)
            ~len:(String.length field - String.length prefix))
        else
          loop rest
  in
  loop fields

let parse_scram_attributes = fun payload -> String.split_on_char ',' payload

let scram_client_final = fun (cfg: Config.t) client_first_bare server_first ->
  let fields = parse_scram_attributes server_first in
  match (field_value "r" fields, field_value "s" fields, field_value "i" fields) with
  | (Some server_nonce, Some salt_b64, Some iterations_text) -> (
      match (Int.parse iterations_text, Encoding.Base64.decode salt_b64) with
      | (Some iterations, Ok salt) when iterations > 0 ->
          let client_final_without_proof = "c=biws,r=" ^ server_nonce in
          let auth_message =
            client_first_bare ^ "," ^ server_first ^ "," ^ client_final_without_proof
          in
          let salted_password = pbkdf2_sha256 ~password:cfg.password ~salt ~iterations in
          let client_key = hmac_sha256 ~key:salted_password ~data:"Client Key" in
          let stored_key = sha256_bytes client_key in
          let client_signature = hmac_sha256 ~key:stored_key ~data:auth_message in
          let client_proof =
            xor_strings client_key client_signature
            |> Bytes.from_string
            |> Encoding.Base64.encode_bytes
          in
          let server_key = hmac_sha256 ~key:salted_password ~data:"Server Key" in
          let server_signature =
            hmac_sha256 ~key:server_key ~data:auth_message
            |> Bytes.from_string
            |> Encoding.Base64.encode_bytes
          in
          Ok (client_final_without_proof ^ ",p=" ^ client_proof, server_signature)
      | (None, _) -> Error (UnexpectedMessage ScramServerFirstInvalidIterationCount)
      | (Some _, Ok _) -> Error (UnexpectedMessage ScramServerFirstInvalidIterationCount)
      | (_, Error _) -> Error (UnexpectedMessage ScramServerFirstInvalidSalt)
    )
  | _ -> Error (UnexpectedMessage ScramServerFirstMissingFields)

let verify_scram_final = fun expected_signature payload ->
  match field_value "v" (parse_scram_attributes payload) with
  | Some signature when signature = expected_signature -> Ok ()
  | Some _ -> Error (UnexpectedMessage ScramServerSignatureMismatch)
  | None -> Error (UnexpectedMessage ScramServerFinalMissingVerifier)

let read_exact = fun stream buf len ->
  let buffer = IO.Buffer.create ~size:len in
  match IO.Reader.read_exact stream.reader ~into:buffer ~len with
  | Error error -> Error (TransportIoError error)
  | Ok () ->
      let chunk = IO.Buffer.to_bytes buffer in
      Bytes.blit_unchecked chunk ~src_offset:0 ~dst:buf ~dst_offset:0 ~len;
      Ok ()

let max_backend_message_body_length = 64 * 1_024 * 1_024

let read_message = fun stream ->
  let header = Bytes.create ~size:5 in
  match read_exact stream header 5 with
  | Error error -> Error error
  | Ok () ->
      let msg_type = Char.code (Option.unwrap (Bytes.get header ~at:0)) in
      let b1 = Char.code (Option.unwrap (Bytes.get header ~at:1)) in
      let b2 = Char.code (Option.unwrap (Bytes.get header ~at:2)) in
      let b3 = Char.code (Option.unwrap (Bytes.get header ~at:3)) in
      let b4 = Char.code (Option.unwrap (Bytes.get header ~at:4)) in
      let length = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in
      let body_len = length - 4 in
      if length < 4 then
        Error (UnexpectedMessage (InvalidBackendMessageLength length))
      else if body_len > max_backend_message_body_length then
        Error (UnexpectedMessage (BackendMessageBodyTooLarge {
          length = body_len;
          limit = max_backend_message_body_length;
        }))
      else if body_len > 0 then
        let body = Bytes.create ~size:body_len in
        match read_exact stream body body_len with
        | Error error -> Error error
        | Ok () -> Ok (msg_type, length, body)
      else
        Ok (msg_type, length, Bytes.create ~size:0)

let parse_backend_message = fun msg_type length body ->
  match Protocol.Reader.parse_backend_message_result msg_type length body with
  | Ok message -> Ok message
  | Error error -> Error (UnexpectedMessage (InvalidBackendMessage error))

let authenticate_cleartext = fun stream (cfg: Config.t) ->
  write_message
    stream
    (Protocol.Writer.password_message cfg.password)

let authenticate_md5 = fun stream (cfg: Config.t) salt ->
  let salt_text = Bytes.to_string salt in
  let inner = md5_hex (cfg.password ^ cfg.user) in
  let password = "md5" ^ md5_hex (inner ^ salt_text) in
  write_message stream (Protocol.Writer.password_message password)

let authenticate_scram_sha256 = fun stream (cfg: Config.t) mechanisms ->
  if not (has_mechanism mechanisms "SCRAM-SHA-256") then
    Error (AuthenticationNotSupported (UnsupportedSaslMechanisms mechanisms))
  else
    let client_nonce = scram_nonce () in
    let client_first_bare = "n=" ^ sasl_escape cfg.user ^ ",r=" ^ client_nonce in
    let client_first = "n,," ^ client_first_bare in
    match write_message
      stream
      (Protocol.Writer.sasl_initial_response ~mechanism:"SCRAM-SHA-256" ~response:client_first) with
    | Error error -> Error error
    | Ok () -> (
        match read_message stream with
        | Error error -> Error error
        | Ok (msg_type, length, body) -> (
            match parse_backend_message msg_type length body with
            | Error error -> Error error
            | Ok (Protocol.AuthenticationSASLContinue server_first) -> (
                match scram_client_final cfg client_first_bare server_first with
                | Error error -> Error error
                | Ok (client_final, server_signature) -> (
                    match write_message stream (Protocol.Writer.sasl_response client_final) with
                    | Error error -> Error error
                    | Ok () -> (
                        match read_message stream with
                        | Error error -> Error error
                        | Ok (msg_type, length, body) -> (
                            match parse_backend_message msg_type length body with
                            | Error error -> Error error
                            | Ok (Protocol.AuthenticationSASLFinal server_final) ->
                                verify_scram_final server_signature server_final
                            | Ok (Protocol.ErrorResponse err) -> Error (ProtocolError err)
                            | Ok _ -> Error (UnexpectedMessage ExpectedScramFinalMessage)
                          )
                      )
                  )
              )
            | Ok (Protocol.ErrorResponse err) -> Error (ProtocolError err)
            | Ok _ -> Error (UnexpectedMessage ExpectedScramContinueMessage)
          )
      )

let perform_handshake = fun stream (cfg: Config.t) ->
  let startup_msg =
    Protocol.Writer.startup_message
      ~user:cfg.user
      ~database:cfg.database
      ~application_name:cfg.application_name
  in
  match write_message stream startup_msg with
  | Error e -> Error e
  | Ok () ->
      let rec read_until_ready () =
        match read_message stream with
        | Error e -> Error e
        | Ok (msg_type, length, body) -> (
            match parse_backend_message msg_type length body with
            | Error error -> Error error
            | Ok backend_msg -> (
                match backend_msg with
                | Protocol.AuthenticationOk -> read_until_ready ()
                | Protocol.AuthenticationCleartextPassword ->
                    authenticate_cleartext stream cfg
                    |> Result.and_then ~fn:read_until_ready
                | Protocol.AuthenticationMD5Password salt ->
                    authenticate_md5 stream cfg salt
                    |> Result.and_then ~fn:read_until_ready
                | Protocol.AuthenticationSASL mechanisms ->
                    authenticate_scram_sha256 stream cfg mechanisms
                    |> Result.and_then ~fn:read_until_ready
                | Protocol.AuthenticationSASLContinue _ ->
                    Error (UnexpectedMessage ScramContinueWithoutSaslStart)
                | Protocol.AuthenticationSASLFinal _ ->
                    Error (UnexpectedMessage ScramFinalWithoutSaslStart)
                | Protocol.ParameterStatus { name; value } ->
                    Log.debug ("PostgreSQL parameter: " ^ name ^ " = " ^ value);
                    read_until_ready ()
                | Protocol.BackendKeyData { process_id; secret_key } ->
                    Log.debug
                      ("Backend key data: pid="
                      ^ string_of_int process_id
                      ^ " secret="
                      ^ string_of_int secret_key);
                    read_until_ready ()
                | Protocol.ReadyForQuery status ->
                    Log.debug ("Ready for query, status: " ^ String.make ~len:1 ~char:status);
                    Ok ()
                | Protocol.ErrorResponse err -> Error (ProtocolError err)
                | Protocol.NoticeResponse err ->
                    Log.info ("PostgreSQL notice: " ^ Protocol.Error.message err);
                    read_until_ready ()
                | _ -> Error (UnexpectedMessage (HandshakeUnexpectedMessageType msg_type))
              )
          )
      in
      read_until_ready ()

(* Initialize connection settings for consistent timestamp parsing *)

let initialize_connection = fun stream ->
  (* Force ISO DateStyle for consistent timestamp format parsing *)
  let datestyle_msg = Protocol.Writer.query_message "SET DateStyle = 'ISO'" in
  (* Force UTC timezone to avoid ambiguity with TIMESTAMP (no TZ) *)
  let timezone_msg = Protocol.Writer.query_message "SET timezone = 'UTC'" in
  let rec drain_responses () =
    match read_message stream with
    | Error e -> Error e
    | Ok (msg_type, length, body) -> (
        match parse_backend_message msg_type length body with
        | Error error -> Error error
        | Ok backend_msg -> (
            match backend_msg with
            | Protocol.ReadyForQuery _ -> Ok ()
            | Protocol.ErrorResponse err -> Error (ProtocolError err)
            | _ -> drain_responses ()
          )
      )
  in
  match write_message stream datestyle_msg with
  | Error e -> Error e
  | Ok () -> (
      match drain_responses () with
      | Error e -> Error e
      | Ok () -> (
          match write_message stream timezone_msg with
          | Error e -> Error e
          | Ok () -> drain_responses ()
        )
    )

let connect = fun (cfg: Config.t) ->
  let id =
    "pg_"
    ^ (
      string_of_int
        (
          Random.int 1_000_000
          |> Result.expect ~msg:"failed to generate client id"
        )
    )
  in
  match Net.Addr.from_host_and_port ~host:cfg.host ~port:cfg.port with
  | Error error -> Error (AddressError error)
  | Ok addr -> (
      match Net.TcpStream.connect addr with
      | Error err -> Error (TransportError err)
      | Ok tcp -> (
          let stream = transport_of_tcp tcp in
          match negotiate_tls stream cfg with
          | Error e ->
              close_transport stream;
              Error e
          | Ok () -> (
              match perform_handshake stream cfg with
              | Error e ->
                  close_transport stream;
                  Error e
              | Ok () ->
                  match initialize_connection stream with
                  | Error e ->
                      close_transport stream;
                      Error e
                  | Ok () ->
                      Ok {
                        id;
                        stream = Some stream;
                        config = cfg;
                        statement_seq = 0;
                        transaction_status = 'I';
                        closed = false;
                      }
            )
        )
    )

let close = fun conn ->
  conn.closed <- true;
  match conn.stream with
  | Some stream -> close_transport stream
  | None -> ()

let ping = fun conn -> not conn.closed

let decode_value = Postgres_value_codec.decode_value

let encode_param = Postgres_value_codec.encode_param

let prepare = fun conn sql ->
  if conn.closed then
    Error ConnectionClosed
  else
    let statement_seq = conn.statement_seq in
    conn.statement_seq <- statement_seq + 1;
  let name = conn.id ^ "_stmt_" ^ string_of_int statement_seq in
  let stmt = { name; sql; conn } in
  Ok stmt

let execute = fun stmt params ->
  if stmt.conn.closed then
    Error ConnectionClosed
  else
    match stmt.conn.stream with
    | None -> Error ConnectionClosed
    | Some stream -> (
        let use_extended_protocol = List.length params > 0 in
        if use_extended_protocol then
          let parse_msg =
            Protocol.Writer.parse_message ~statement_name:stmt.name ~query:stmt.sql ~param_types:[]
          in
          let describe_msg = Protocol.Writer.describe_message ~what:'S' ~name:stmt.name in
          let encoded_params = List.map params ~fn:encode_param in
          let bind_msg =
            Protocol.Writer.bind_message
              ~portal_name:""
              ~statement_name:stmt.name
              ~params:encoded_params
          in
          let execute_msg = Protocol.Writer.execute_message ~portal_name:"" ~max_rows:0 in
          let sync_msg = Protocol.Writer.sync_message () in
          let send_messages =
            let* () = write_message stream parse_msg in
            let* () = write_message stream describe_msg in
            let* () = write_message stream bind_msg in
            let* () = write_message stream execute_msg in
            write_message stream sync_msg
          in
          match send_messages with
          | Error e -> Error e
          | Ok () ->
              let result_set = { rows = Collections.Queue.create (); rows_affected = 0 } in
              let column_info = ref [] in
              let rec read_extended_results () =
                match read_message stream with
                | Error e -> Error e
                | Ok (msg_type, length, body) -> (
                    match parse_backend_message msg_type length body with
                    | Error error -> Error error
                    | Ok backend_msg -> (
                        match backend_msg with
                        | Protocol.ParseComplete -> read_extended_results ()
                        | Protocol.BindComplete -> read_extended_results ()
                        | Protocol.RowDescription row_desc ->
                            column_info := row_desc;
                            read_extended_results ()
                        | Protocol.DataRow cols ->
                            let row =
                              if List.length !column_info = List.length cols then
                                List.zip !column_info cols
                                |> List.map
                                  ~fn:(fun ((field: Protocol.Row.field), row_val) ->
                                    let decoded_value =
                                      match row_val with
                                      | Protocol.Row.Null -> Sqlx_driver.Value.null
                                      | Protocol.Row.Value value -> decode_value field value
                                    in
                                    (field.name, decoded_value))
                              else
                                List.enumerate cols
                                |> List.map
                                  ~fn:(fun (index, row_val) ->
                                    let value =
                                      match row_val with
                                      | Protocol.Row.Null -> Sqlx_driver.Value.null
                                      | Protocol.Row.Value v -> Sqlx_driver.Value.string v
                                    in
                                    ("col_" ^ string_of_int index, value))
                            in
                            Collections.Queue.push result_set.rows ~value:row;
                            read_extended_results ()
                        | Protocol.CommandComplete tag ->
                            Log.debug ("Command complete: " ^ tag);
                            let parts = String.split_on_char ' ' tag in
                            (
                              match List.rev parts with
                              | n :: _ -> (
                                  match Int.parse n with
                                  | Some count -> result_set.rows_affected <- count
                                  | None -> ()
                                )
                              | [] -> ()
                            );
                            read_extended_results ()
                        | Protocol.ReadyForQuery status ->
                            stmt.conn.transaction_status <- status;
                            Ok result_set
                        | Protocol.ErrorResponse err -> Error (ProtocolError err)
                        | Protocol.NoticeResponse err ->
                            Log.info ("PostgreSQL notice: " ^ Protocol.Error.message err);
                            read_extended_results ()
                        | Protocol.NoData -> read_extended_results ()
                        | Protocol.EmptyQueryResponse -> Ok result_set
                        | _ -> read_extended_results ()
                      )
                  )
              in
              read_extended_results ()
        else
          let query_msg = Protocol.Writer.query_message stmt.sql in
          match write_message stream query_msg with
          | Error e -> Error e
          | Ok () ->
              let result_set = { rows = Collections.Queue.create (); rows_affected = 0 } in
              let column_info = ref [] in
              let rec read_query_results () =
                match read_message stream with
                | Error e -> Error e
                | Ok (msg_type, length, body) -> (
                    match parse_backend_message msg_type length body with
                    | Error error -> Error error
                    | Ok backend_msg -> (
                        match backend_msg with
                        | Protocol.RowDescription row_desc ->
                            column_info := row_desc;
                            read_query_results ()
                        | Protocol.DataRow cols ->
                            let row =
                              if List.length !column_info = List.length cols then
                                List.zip !column_info cols
                                |> List.map
                                  ~fn:(fun ((field: Protocol.Row.field), row_val) ->
                                    let decoded_value =
                                      match row_val with
                                      | Protocol.Row.Null -> Sqlx_driver.Value.null
                                      | Protocol.Row.Value value -> decode_value field value
                                    in
                                    (field.name, decoded_value))
                              else
                                List.enumerate cols
                                |> List.map
                                  ~fn:(fun (index, row_val) ->
                                    let value =
                                      match row_val with
                                      | Protocol.Row.Null -> Sqlx_driver.Value.null
                                      | Protocol.Row.Value v -> Sqlx_driver.Value.string v
                                    in
                                    ("col_" ^ string_of_int index, value))
                            in
                            Collections.Queue.push result_set.rows ~value:row;
                            read_query_results ()
                        | Protocol.CommandComplete tag ->
                            Log.debug ("Command complete: " ^ tag);
                            let parts = String.split_on_char ' ' tag in
                            (
                              match List.rev parts with
                              | n :: _ -> (
                                  match Int.parse n with
                                  | Some count -> result_set.rows_affected <- count
                                  | None -> ()
                                )
                              | [] -> ()
                            );
                            read_query_results ()
                        | Protocol.ReadyForQuery status ->
                            stmt.conn.transaction_status <- status;
                            Ok result_set
                        | Protocol.ErrorResponse err -> Error (ProtocolError err)
                        | Protocol.NoticeResponse err ->
                            Log.info ("PostgreSQL notice: " ^ Protocol.Error.message err);
                            read_query_results ()
                        | _ -> Error (UnexpectedMessage (QueryUnexpectedMessageType msg_type))
                      )
                  )
              in
              read_query_results ()
      )

let fetch_row = fun result_set -> Collections.Queue.pop result_set.rows

let rows_affected = fun result_set -> result_set.rows_affected

let prepare_migration = fun sql -> Ok [ sql ]

let execute_simple_command = fun conn sql ->
  match prepare conn sql with
  | Error error -> Error error
  | Ok stmt -> (
      match execute stmt [] with
      | Error error -> Error error
      | Ok _ -> Ok ()
    )

let begin_transaction = fun conn ->
  if conn.closed then
    Error ConnectionClosed
  else if conn.transaction_status != 'I' then
    Error (UnexpectedMessage TransactionAlreadyInProgress)
  else
    execute_simple_command conn "BEGIN"

let commit = fun conn ->
  if conn.closed then
    Error ConnectionClosed
  else if conn.transaction_status != 'T' then
    Error (UnexpectedMessage NoTransactionInProgress)
  else
    execute_simple_command conn "COMMIT"

let rollback = fun conn ->
  if conn.closed then
    Error ConnectionClosed
  else if conn.transaction_status != 'T' then
    Error (UnexpectedMessage NoTransactionInProgress)
  else
    execute_simple_command conn "ROLLBACK"

let isolation_level_sql = fun level ->
  match level with
  | Sqlx_driver.Driver.ReadUncommitted -> "READ UNCOMMITTED"
  | Sqlx_driver.Driver.ReadCommitted -> "READ COMMITTED"
  | Sqlx_driver.Driver.RepeatableRead -> "REPEATABLE READ"
  | Sqlx_driver.Driver.Serializable -> "SERIALIZABLE"

let set_isolation_level = fun conn level ->
  if conn.closed then
    Error ConnectionClosed
  else
    let sql =
      if conn.transaction_status = 'T' then
        "SET TRANSACTION ISOLATION LEVEL " ^ isolation_level_sql level
      else
        "SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL " ^ isolation_level_sql level
    in
    execute_simple_command conn sql
