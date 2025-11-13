open Std
open Std.IO

type t =
  | Conn : {
      protocol : string option;
      stream : Net.TcpStream.t;
      peer : Net.Addr.stream_addr;
      default_read_size : int;
      accepted_at : Time.Instant.t;
      connected_at : Time.Instant.t;
    }
      -> t

let make ?(protocol = None) ~accepted_at ~stream ~buffer_size ~peer () =
  Conn
    {
      stream;
      protocol;
      peer;
      default_read_size = buffer_size;
      accepted_at;
      connected_at = Time.Instant.now ();
    }

let negotiated_protocol (Conn t) = t.protocol

let receive ?(limit = 1024) ?read_size ?timeout (Conn { default_read_size; stream; _ }) =
  let read_size = Option.unwrap_or ~default:default_read_size read_size in
  Log.trace ("receive with read_size of " ^ string_of_int read_size ^ " (using limit=" ^ string_of_int limit ^ ")");
  let capacity = Int.min limit read_size in
  let buf = Bytes.create capacity in
  match Net.TcpStream.read stream buf ?timeout () with
  | Ok 0 -> Error `Closed
  | Ok n -> Ok (Bytes.sub_string buf 0 n)
  | Error _ -> Error `Closed

let rec send conn data =
  Log.trace ("will send " ^ string_of_int (String.length data) ^ " bytes");
  match do_send conn data with
  | Ok () ->
      Log.trace ("sent " ^ string_of_int (String.length data) ^ " bytes");
      Ok ()
  | Error e -> Error e

and do_send (Conn { stream; _ }) data =
  let buf = Bytes.of_string data in
  match Net.TcpStream.write stream buf () with
  | Ok _n -> Ok ()
  | Error _ -> Error `Closed

let peer (Conn { peer; _ }) = peer
let connected_at (Conn { connected_at; _ }) = connected_at
let accepted_at (Conn { accepted_at; _ }) = accepted_at
let stream (Conn { stream; _ }) = stream

let close (Conn { stream; _ }) =
  Net.TcpStream.close stream

let send_file (Conn _) ?off:_ ~len:_ _path =
  (* TODO: implement sendfile optimization *)
  Ok ()
