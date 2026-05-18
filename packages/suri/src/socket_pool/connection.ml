open Std
open Std.IO

type t =
  | Conn: {
      protocol: string option;
      stream: Net.TcpStream.t;
      peer: Net.Addr.stream_addr;
      default_read_size: int;
      accepted_at: Time.Instant.t;
      connected_at: Time.Instant.t;
    } -> t

type send_file_range_error = { off: int; len: int; size: int }

type error =
  | Closed
  | ReadError of Net.TcpStream.error
  | WriteError of Net.TcpStream.error
  | FileError of Fs.error
  | InvalidRange of send_file_range_error

type send_file_error = error

let tcp_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Net.TcpStream.Connection_refused -> "connection refused"
  | Net.TcpStream.Closed -> "closed"
  | Net.TcpStream.System_error error -> IO.error_message error

let error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Closed -> "connection closed"
  | ReadError error -> "read failed: " ^ tcp_error_to_string error
  | WriteError error -> "write failed: " ^ tcp_error_to_string error
  | FileError error -> "file failed: " ^ IO.error_message error
  | InvalidRange { off; len; size } ->
      "invalid file range: off="
      ^ string_of_int off
      ^ " len="
      ^ string_of_int len
      ^ " size="
      ^ string_of_int size

let make = fun ?(protocol = None) ~accepted_at ~stream ~buffer_size ~peer () ->
  Conn {
    stream;
    protocol;
    peer;
    default_read_size = buffer_size;
    accepted_at;
    connected_at = Time.Instant.now ();
  }

let negotiated_protocol = fun (Conn t) -> t.protocol

let receive = fun ?limit ?read_size ?timeout (Conn { default_read_size; stream; _ }) ->
  let read_size = Option.unwrap_or ~default:default_read_size read_size in
  let limit = Option.unwrap_or ~default:read_size limit in
  Log.trace
    ("receive with read_size of "
    ^ string_of_int read_size
    ^ " (using limit="
    ^ string_of_int limit
    ^ ")");
  let capacity = Int.min limit read_size in
  let buf = Bytes.create ~size:capacity in
  match Net.TcpStream.read stream buf ?timeout () with
  | Ok 0 -> Error Closed
  | Ok n ->
      Ok (
        Bytes.sub_unchecked buf ~offset:0 ~len:n
        |> Bytes.to_string
      )
  | Error error -> Error (ReadError error)

let write_all_with = fun ~write data ->
  let buf = Bytes.from_string data in
  let total = String.length data in
  let rec loop pos =
    if pos >= total then
      Ok ()
    else
      match write buf ~pos ~len:(total - pos) with
      | Ok n when n > 0 -> loop (pos + n)
      | Ok _ -> Error Closed
      | Error error -> Error (WriteError error)
  in
  loop 0

let rec send = fun conn data ->
  Log.trace ("will send " ^ string_of_int (String.length data) ^ " bytes");
  match do_send conn data with
  | Ok () ->
      Log.trace ("sent " ^ string_of_int (String.length data) ^ " bytes");
      Ok ()
  | Error e -> Error e

and do_send = fun (Conn { stream; _ }) data ->
  write_all_with
    data
    ~write:(fun buf ~pos ~len ->
      Net.TcpStream.write stream buf ~pos ~len ())

let peer = fun (Conn { peer; _ }) -> peer

let connected_at = fun (Conn { connected_at; _ }) -> connected_at

let accepted_at = fun (Conn { accepted_at; _ }) -> accepted_at

let stream = fun (Conn { stream; _ }) -> stream

let close = fun (Conn { stream; _ }) -> Net.TcpStream.close stream

let send_file_slice = fun ?(off = 0) ~len content ->
  let size = String.length content in
  if off < 0 || len < 0 || off > size || len > size - off then
    Error (InvalidRange { off; len; size })
  else
    Ok (String.sub content ~offset:off ~len)

let send_file = fun conn ?(off = 0) ~len path ->
  match Fs.read (Path.v path) with
  | Error error -> Error (FileError error)
  | Ok content -> (
      match send_file_slice ~off ~len content with
      | Error error -> Error error
      | Ok chunk -> send conn chunk
    )
