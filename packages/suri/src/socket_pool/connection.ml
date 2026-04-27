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

let receive = fun ?(limit = 1_024) ?read_size ?timeout (Conn { default_read_size; stream; _ }) ->
  let read_size = Option.unwrap_or ~default:default_read_size read_size in
  Log.trace
    ("receive with read_size of "
    ^ string_of_int read_size
    ^ " (using limit="
    ^ string_of_int limit
    ^ ")");
  let capacity = Int.min limit read_size in
  let buf = Bytes.create ~size:capacity in
  match Net.TcpStream.read stream buf ?timeout () with
  | Ok 0 -> Error `Closed
  | Ok n ->
      Ok (
        Bytes.sub_unchecked buf ~offset:0 ~len:n
        |> Bytes.to_string
      )
  | Error _ -> Error `Closed

let write_all_with = fun ~write data ->
  let buf = Bytes.from_string data in
  let total = String.length data in
  let rec loop pos =
    if pos >= total then
      Ok ()
    else
      match write buf ~pos ~len:(total - pos) with
      | Ok n when n > 0 -> loop (pos + n)
      | Ok _ -> Error `Closed
      | Error _ -> Error `Closed
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

let send_file = fun (Conn _) ?off:_ ~len:_ _path ->
  (* TODO: implement sendfile optimization *)
  Ok ()

module For_testing = struct
  let write_all_with = write_all_with
end
