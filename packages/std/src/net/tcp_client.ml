(** TCP client for line-based protocols *)
open Global
open IO

type t = {
  stream: Kernel.Net.TcpStream.t;
  mutable leftover: string;
  (* Buffer for data read past newline *)
}

type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

let io_error_of_addr_error = fun __tmp1 ->
  match __tmp1 with
  | Addr.System_error error -> error
  | Addr.Invalid_port_number value -> IO.Unknown_error ("invalid port: " ^ value)
  | Addr.Invalid_format value -> IO.Unknown_error ("invalid address: " ^ value)

let connect = fun ~host ~port ->
  match Addr.from_host_and_port ~host ~port with
  | Error err -> Error (System_error (io_error_of_addr_error err))
  | Ok addr -> (
      match Tcp_stream.connect addr with
      | Ok stream -> Ok { stream; leftover = "" }
      | Error Tcp_stream.Closed -> Error Closed
      | Error (Tcp_stream.System_error io_err) -> Error (System_error io_err)
      | Error Tcp_stream.Connection_refused -> Error Connection_refused
    )

let send = fun t data ->
  let buffer = Bytes.from_string data in
  let len = Bytes.length buffer in
  let rec send_all pos =
    if pos >= len then
      Ok ()
    else
      match Tcp_stream.write t.stream buffer ~pos ~len:(len - pos) () with
      | Ok bytes_written -> send_all (pos + bytes_written)
      | Error e ->
          Error (
            "Send failed: " ^ (
              match e with
              | Closed -> "connection closed"
              | System_error io_err -> IO.error_message io_err
              | Connection_refused -> "connection refused"
            )
          )
  in
  send_all 0

let receive = fun t ->
  (* Check if we already have a complete line in leftover buffer *)
  match String.index_of t.leftover ~char:'\n' with
  | Some idx ->
      (* Found newline in leftover, return line and save remainder *)
      let line = String.sub t.leftover ~offset:0 ~len:idx in
      let remainder_start = idx + 1 in
      let remainder_len = String.length t.leftover - remainder_start in
      t.leftover <- (
        if remainder_len > 0 then
          String.sub t.leftover ~offset:remainder_start ~len:remainder_len
        else
          ""
      );
      Ok line
  | None ->
      (* No complete line in leftover, need to read more *)
      let buffer = Bytes.create ~size:4_096 in
      let buffer_size = Bytes.length buffer in
      (* Read until we get a newline *)
      let rec read_line acc =
        match Tcp_stream.read t.stream buffer ~pos:0 ~len:buffer_size () with
        | Ok bytes_read -> (
            let data = Bytes.to_string (Bytes.sub_unchecked buffer ~offset:0 ~len:bytes_read) in
            let full_data = acc ^ data in
            (* Check if we have a complete line *)
            match String.index_of full_data ~char:'\n' with
            | Some idx ->
                (* Found newline, save remainder and return line *)
                let line = String.sub full_data ~offset:0 ~len:idx in
                let remainder_start = idx + 1 in
                let remainder_len = String.length full_data - remainder_start in
                t.leftover <- (
                  if remainder_len > 0 then
                    String.sub full_data ~offset:remainder_start ~len:remainder_len
                  else
                    ""
                );
                Ok line
            | None ->
                (* No newline yet, keep reading *)
                read_line full_data
          )
        | Error Closed ->
            if acc = "" && t.leftover = "" then
              Error "Connection closed"
            else
              (* Return what we have, clear leftover *)
              let result = t.leftover ^ acc in
              t.leftover <- "";
            Ok result
        | Error (System_error io_err) -> Error (IO.error_message io_err)
        | Error Connection_refused -> Error "Connection refused"
      in
      read_line t.leftover

let close = fun t -> Tcp_stream.close t.stream
