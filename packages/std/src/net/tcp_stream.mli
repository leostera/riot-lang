(** TCP stream for connected sockets *)

open Global

type t = Kernel.Net.Tcp_stream.t
(** Connect to a TCP endpoint. This will suspend the process until the
    connection is established. *)
type error =
  | Connection_refused
  | Closed
  | System_error of IO.error
val connect : Kernel.Net.Addr.stream_addr -> (t, error) result

(** Read data from the stream. This will suspend the process until data is
    available. Returns the number of bytes read. 
    
    @param timeout Optional timeout duration. If specified and no data arrives
                   within the timeout, raises [Syscall_timeout]. *)
val read : t -> bytes -> ?pos:int -> ?len:int -> ?timeout:Time.Duration.t -> unit -> (int, error) result

(** Write data to the stream. This will suspend the process until the socket is
    ready for writing. Returns the number of bytes written. *)
val write : t -> bytes -> ?pos:int -> ?len:int -> unit -> (int, error) result

(** Close the stream *)
val close : t -> unit

(** [to_reader stream] creates a Reader from the TCP stream.

    The reader wraps the stream's read operations in the generic IO.Reader
    interface, allowing it to be used with any code that accepts readers.

    Example:
    {[
      let stream = Net.TcpStream.connect addr |> Result.unwrap in
      let reader = Net.TcpStream.to_reader stream in

      let buf = Bytes.create 4096 in
      match IO.read reader buf with
      | Ok n -> process_data (Bytes.sub buf 0 n)
      | Error `Closed -> handle_closed ()
      | Error (`System_error msg) -> handle_error msg
    ]} *)
val to_reader : t -> (t, error) IO.Reader.t

(** [to_writer stream] creates a Writer from the TCP stream.

    The writer wraps the stream's write operations in the generic IO.Writer
    interface, allowing it to be used with any code that accepts writers.

    Example:
    {[
      let stream = Net.TcpStream.connect addr |> Result.unwrap in
      let writer = Net.TcpStream.to_writer stream in

      match IO.write_all writer ~buf:"Hello, world!\n" with
      | Ok () -> println "Data sent"
      | Error `Closed -> handle_closed ()
      | Error (`System_error msg) -> handle_error msg
    ]} *)
val to_writer : t -> (t, error) IO.Writer.t
