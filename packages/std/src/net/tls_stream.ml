(** TLS stream for encrypted connections *)
open Global
open IO

(* Result monad for cleaner error handling *)

let ( let* ) = fun x f -> Result.and_then x ~fn:f

type error =
  | Closed
  | Handshake_failed of string
  | System_error of IO.error
  | Network_read_failed of IO.error
  | Network_write_failed of IO.error
  | Tls_not_available
  | Unsupported_vectored_operation

let io_error_of_tls_error = function
  | Closed -> IO.Closed
  | Handshake_failed message -> IO.Unknown_error ("tls handshake failed: " ^ message)
  | System_error error -> error
  | Network_read_failed error -> error
  | Network_write_failed error -> error
  | Tls_not_available -> IO.Unknown_error "tls not available"
  | Unsupported_vectored_operation -> IO.Unknown_error "tls stream does not support vectored io"

type 'src t = {
  reader: IO.Reader.t;
  writer: IO.Writer.t;
  engine: Tls.engine;
  mutable state: [`Active | `Eof | `Error of exn];
  network_in_buf: bytes;
  network_out_buf: bytes;
}

(* Helper: Read encrypted data from network and pump into TLS engine *)

let read_from_network = fun t ->
  let buffer = IO.Buffer.create ~size:(Bytes.length t.network_in_buf) in
  match IO.read t.reader ~into:buffer with
  | Ok n ->
      if n = 0 then
        Error Closed
      else
        (
          let chunk = IO.Buffer.to_bytes buffer in
          Bytes.blit_unchecked chunk ~src_offset:0 ~dst:t.network_in_buf ~dst_offset:0 ~len:n;
          let _ = Tls.pump_encrypted_in t.engine t.network_in_buf ~pos:0 ~len:n in
          Ok ()
        )
  | Error err -> Error (Network_read_failed err)

(* Already wrapped by map_err *)

(* Helper: Flush encrypted data from TLS engine to network *)

let flush_to_network = fun t ->
  let rec flush_loop () =
    let n = Tls.read_encrypted_out t.engine t.network_out_buf in
    if n = 0 then
      Ok ()
    else
      (
        let data = Bytes.sub_unchecked t.network_out_buf ~offset:0 ~len:n in
        match IO.write_all t.writer ~from:(IO.Buffer.from_bytes data) with
        | Ok () -> flush_loop ()
        | Error err -> Error (Network_write_failed err)
      )
  in
  flush_loop ()

(* Perform TLS handshake *)

let do_handshake = fun t ->
  let rec handshake_loop () =
    if Tls.handshake_complete t.engine then
      Ok ()
    else
      (
        (* Call do_handshake to advance the state machine *)
        match Tls.do_handshake t.engine with
        | Handshake_done -> Ok ()
        | Need_network_read ->
            (* TLS needs encrypted data from network *)
            (* But first, flush any pending output (e.g., ClientHello) *)
            let* () = flush_to_network t in let* () = read_from_network t in handshake_loop ()
        | Need_network_write ->
            (* TLS needs to send encrypted data to network *)
            let* () = flush_to_network t in handshake_loop ()
      )
  in
  handshake_loop ()

let of_client_io = fun ~reader ~writer ~hostname () ->
  (* Initialize OpenSSL if not already done *)
  (
    try Tls.init () with
    | _ -> ()
  );
  (* Check if TLS is available *)
  if not (Tls.is_available ()) then
    Error Tls_not_available
  else
    let engine = Tls.create_client_engine ~hostname in
    let t = {
      reader;
      writer;
      engine;
      state = `Active;
      network_in_buf = Bytes.create ~size:16_384;
      network_out_buf = Bytes.create ~size:16_384;
    }
    in
    match do_handshake t with
    | Ok () -> Ok t
    | Error e -> Error e

let of_server_io = fun ~reader ~writer ~cert_file ~key_file () ->
  (* Initialize OpenSSL if not already done *)
  (
    try Tls.init () with
    | _ -> ()
  );
  if not (Tls.is_available ()) then
    Error Tls_not_available
  else
    let engine = Tls.create_server_engine ~cert_file ~key_file in
    let t = {
      reader;
      writer;
      engine;
      state = `Active;
      network_in_buf = Bytes.create ~size:16_384;
      network_out_buf = Bytes.create ~size:16_384;
    }
    in
    match do_handshake t with
    | Ok () -> Ok t
    | Error e -> Error e

let of_tcp_socket = fun ~mode sock ->
  let reader = Tcp_stream.to_reader sock in
  let writer = Tcp_stream.to_writer sock in
  match mode with
  | `Client hostname -> of_client_io ~reader ~writer ~hostname ()
  | `Server (cert_file, key_file) -> of_server_io ~reader ~writer ~cert_file ~key_file ()

let of_tcp_client = fun ~hostname tcp -> of_tcp_socket ~mode:(`Client hostname) tcp

let of_tcp_server = fun ~cert_file ~key_file tcp -> of_tcp_socket
  ~mode:(`Server (cert_file, key_file))
  tcp

(* Read plaintext from TLS stream *)

let read_plaintext t dst: (int, error) Result.t =
  let rec read_loop () =
    match Tls.read_decrypted t.engine dst ~pos:0 ~len:(Bytes.length dst) with
    | Read n -> Ok n
    | Eof -> Ok 0
    | Need_network_read ->
        (* TLS needs more encrypted data from network *)
        let* () = read_from_network t in read_loop ()
    | Need_network_write ->
        (* TLS needs to send encrypted data to network *)
        let* () = flush_to_network t in read_loop ()
  in
  match t.state with
  | `Eof -> Ok 0
  | `Error e -> raise e
  | `Active -> (
      match read_loop () with
      | Ok 0 ->
          t.state <- `Eof;
          Ok 0
      | Ok n -> Ok n
      | Error e ->
          t.state <- `Error (Failure "TLS read error");
          Error e
    )

(* Write plaintext to TLS stream *)

let write_plaintext_bytes t src_bytes: (int, error) Result.t =
  let src_len = Bytes.length src_bytes in
  let rec write_loop pos remaining =
    if remaining = 0 then
      Ok src_len
    else
      match Tls.write_plaintext t.engine src_bytes ~pos ~len:remaining with
      | Written n ->
          (* Flush encrypted data to network *)
          let* () = flush_to_network t in write_loop (pos + n) (remaining - n)
      | Need_network_read ->
          (* SSL needs more input (renegotiation?) *)
          let* () = read_from_network t in write_loop pos remaining
      | Need_network_write ->
          (* Need to flush first *)
          let* () = flush_to_network t in write_loop pos remaining
  in
  match t.state with
  | `Eof -> Error Closed
  | `Error e -> raise e
  | `Active -> (
      match write_loop 0 src_len with
      | Ok n -> Ok n
      | Error e ->
          t.state <- `Error (Failure "TLS write error");
          Error e
    )

let write_plaintext t src: (int, error) Result.t = write_plaintext_bytes t (Bytes.from_string src)

(* Expose as reader *)

let to_reader: type src. src t -> IO.Reader.t = fun tls_stream ->
  let module Read = struct
    type nonrec t = src t

    let read = fun (tls: t) ~into ->
      let writable =
        if IO.Buffer.writable_bytes into = 0 then
          (
            match IO.Buffer.ensure_free into 4_096 with
            | Ok () -> IO.Buffer.writable into
            | Error error ->
                Kernel.SystemError.panic
                  ("Net.TlsStream.to_reader.ensure_free: " ^ Kernel.IO.Error.message error)
          )
        else
          IO.Buffer.writable into
      in
      let scratch = Bytes.create ~size:(IO.IoSlice.length writable) in
      match read_plaintext tls scratch with
      | Ok count ->
          let chunk = Bytes.sub_unchecked scratch ~offset:0 ~len:count in
          begin
            match IO.Buffer.append_bytes into chunk with
            | Ok () -> Ok count
            | Error error ->
                Kernel.SystemError.panic
                  ("Net.TlsStream.to_reader.append: " ^ Kernel.IO.Error.message error)
          end
      | Error err -> Error (io_error_of_tls_error err)

    let read_vectored = fun (tls: t) ~into ->
      let total = IO.IoVec.length into in
      if total = 0 then
        Ok 0
      else
        let scratch = Bytes.create ~size:total in
        match read_plaintext tls scratch with
        | Ok count ->
            let copied = ref 0 in
            IO.IoVec.for_each
              into
              ~fn:(fun segment ->
                if !copied < count then
                  let remaining = count - !copied in
                  let available = IO.IoSlice.length segment in
                  let chunk =
                    if available < remaining then
                      available
                    else
                      remaining
                  in
                  if chunk > 0 then (
                    IO.IoSlice.blit_from_bytes_unchecked
                      scratch
                      ~src_off:!copied
                      segment
                      ~dst_off:0
                      ~len:chunk;
                    copied := !copied + chunk
                  ));
            Ok count
        | Error err -> Error (io_error_of_tls_error err)

    let is_read_vectored = fun _t -> false
  end in
  IO.Reader.from_source (module Read) tls_stream

(* Expose as writer *)

let to_writer: type src. src t -> IO.Writer.t = fun tls ->
  let module Write = struct
    type nonrec t = src t

    let write = fun t ~from ->
      match write_plaintext_bytes t (IO.Buffer.to_bytes from) with
      | Ok written -> Ok written
      | Error err -> Error (io_error_of_tls_error err)

    let write_vectored = fun t ~from ->
      match write_plaintext_bytes t (IO.IoVec.to_bytes from) with
      | Ok written -> Ok written
      | Error err -> Error (io_error_of_tls_error err)

    let flush = fun _t -> Ok ()
  end in
  IO.Writer.from_sink (module Write) tls

(* TLS information *)

let alpn_protocol = fun t -> Tls.alpn_protocol t.engine

let close = fun _t -> ()
(* Don't close underlying transport *)
