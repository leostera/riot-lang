open Global0

(** {1 Writer}

    Generic abstraction for writable data destinations.

    Writer provides a uniform interface for writing to any destination (files,
    sockets, in-memory buffers, etc.) through a first-class module wrapper. This
    allows code to be polymorphic over the destination of data and error types.

    {2 Design}

    The Writer abstraction uses first-class modules to provide type erasure
    while maintaining type safety. Destinations implement the [Write] signature,
    then get wrapped in a [Writer.t] for uniform handling.

    The Writer is parametrized by both the destination type ['dst] and error
    type ['err], allowing each destination to define its own error types without
    forcing a common error hierarchy.

    The abstraction handles partial writes automatically through [write_all],
    which is the recommended way to ensure all data is written.

    {2 Example}

    Creating a writer from a TCP stream:
    {[
      let stream = Net.TcpStream.connect addr in
      let writer = Net.TcpStream.to_writer stream in

      match IO.write_all writer ~buf:"Hello, world!\n" with
      | Ok () -> print_endline "Data sent"
      | Error `Closed -> print_endline "Connection closed"
      | Error e -> handle_error e
    ]}

    Writing with explicit control:
    {[
      let data = "Large data chunk..." in
      let rec write_loop offset =
        if offset >= String.length data then Ok ()
        else
          let remaining =
            String.sub data offset (String.length data - offset)
          in
          match IO.write writer ~buf:remaining with
          | Ok n -> write_loop (offset + n)
          | Error e -> Error e
      in
      write_loop 0
    ]} *)

module type Write = sig
  (** Interface that writable destinations must implement.

      Destinations like TCP streams, files, or in-memory buffers implement this
      signature to become writable through the Writer abstraction. *)
  type t
  (** The type of the writable destination *)
  type err

  (** The error type for this destination *)
  val write: t -> buf:string -> (int, err) result

  (** [write dst ~buf] writes data from [buf] to [dst].

      @return
        Number of bytes actually written. May be less than [String.length buf]
        if the destination cannot accept all data immediately.
      @raise Never raises - all errors are returned as [Error].

      The implementation should:
      - Write as much data as possible from [buf]
      - Return the actual number of bytes written
      - Return [Error] with appropriate error if the destination is closed
      - Suspend the calling process if the destination is not ready (for async
        destinations)

      Note: This may write less than the full buffer. Use {!write_all} to ensure
      all data is written. *)
  val write_owned_vectored: t -> bufs:Iovec.t -> (int, err) result

  (** [write_owned_vectored dst ~bufs] writes data from multiple buffers atomically.
      
      This is more efficient than multiple [write] calls when gathering data
      from multiple buffers. The implementation can use system calls like
      [writev] for optimal performance.
      
      @return Total number of bytes written across all buffers
      @see {!Iovec} for IO vector operations
  *)
  val flush: t -> (unit, err) result

  (** [flush dst] ensures all buffered data is written to the destination.

      For unbuffered destinations (like TCP sockets), this is typically a no-op.
      For buffered destinations (like files), this forces pending data to be
      written to the underlying storage.

      @return [Ok ()] on success, [Error] on failure *)
end

type ('dst, 'err) write = (module Write with type t = 'dst and type err = 'err)
(** First-class module type for writable destinations.

    This allows destinations to be passed as values while maintaining their
    implementation. Type parameters:
    - ['dst] is the concrete destination type
    - ['err] is the error type for this destination *)
type ('dst, 'err) t

(** Writer wrapping a writable destination.

    This is an existential type that hides the concrete destination type while
    preserving its capabilities. Code working with [Writer.t] doesn't need to
    know the specific destination implementation.

    Type parameters:
    - ['dst] is the underlying destination type (e.g.,
      [Kernel.Net.Tcp_stream.t])
    - ['err] is the error type for write operations *)
val of_write_src: ('dst, 'err) write -> 'dst -> ('dst, 'err) t

(** [of_write_src (module Write) dst] creates a writer from a destination.

    This is typically called by destination modules (like
    [Net.TcpStream.to_writer]) rather than by user code.

    Example implementation:
    {[
      let to_writer stream =
        let module Write = struct
          type t = Tcp_stream.t
          type err = Tcp_stream.error

          let write t ~buf = Tcp_stream.write t (Bytes.of_string buf)
          let write_owned_vectored = Tcp_stream.write_vectored
          let flush _t = Ok ()
        end in
        IO.Writer.of_write_src (module Write) stream
    ]} *)
val write: ('dst, 'err) t -> buf:string -> (int, 'err) result

(** [write writer ~buf] writes data to the writer.

    @return
      Number of bytes actually written (may be less than [String.length buf])

    This performs a single write operation and may not write all the data. For
    most use cases, {!write_all} is preferred to ensure all data is written.

    Example:
    {[
      match IO.write writer ~buf:"Hello" with
      | Ok n -> Printf.printf "Wrote %d bytes\n" n
      | Error e -> handle_error e
    ]} *)
val write_all: ('dst, 'err) t -> buf:string -> (unit, 'err) result

(** [write_all writer ~buf] writes all data, retrying as needed.

    This is the recommended way to write data. It handles partial writes
    automatically by retrying until all data is written or an error occurs.

    @return [Ok ()] when all data has been written, [Error] on failure

    Example:
    {[
      match IO.write_all writer ~buf:"Hello, world!\n" with
      | Ok () -> print_endline "All data written"
      | Error e -> handle_error e
    ]}

    This is equivalent to:
    {[
      let rec write_loop buf =
        if String.length buf = 0 then Ok ()
        else
          match IO.write writer ~buf with
          | Ok n -> write_loop (String.sub buf n (String.length buf - n))
          | Error e -> Error e
      in
      write_loop original_buf
    ]} *)
val write_owned_vectored: ('dst, 'err) t -> bufs:Iovec.t -> (int, 'err) result

(** [write_owned_vectored writer ~bufs] writes data from multiple buffers.

    More efficient than multiple [write] calls when gathering data from multiple
    buffers into a single write operation.

    @return Total number of bytes written across all buffers

    Example:
    {[
      let header = Bytes.of_string "HTTP/1.1 200 OK\r\n" in
      let body = Bytes.of_string "Hello, world!" in
      let iov =
        Iovec.create ~count:2 ~size:(Bytes.length header + Bytes.length body) ()
      in
      match IO.write_owned_vectored writer ~bufs:iov with
      | Ok n -> Printf.printf "Wrote %d bytes total\n" n
      | Error e -> handle_error e
    ]} *)
val write_all_vectored: ('dst, 'err) t -> bufs:Iovec.t -> (unit, 'err) result

(** [write_all_vectored writer ~bufs] writes all data from IO vectors.

    Repeatedly calls [write_owned_vectored] until all data in [bufs] is written
    or an error occurs.

    @param bufs IO vector containing data to write
    @return [Ok ()] if all data written, [Error] on failure

    Example:
    {[
      let iov = Iovec.create ~count:2 ~size:10 () in
      Iovec.set iov 0 (Bytes.of_string "Hello");
      Iovec.set iov 1 (Bytes.of_string "World");
      match IO.write_all_vectored writer ~bufs:iov with
      | Ok () -> print_endline "All data written"
      | Error e -> handle_error e
    ]} *)
val map_err: ('dst, 'a) t -> fn:('a -> 'b) -> ('dst, 'b) t

(** [map_err writer ~fn] transforms the error type of a writer.
    
    Useful for wrapping errors from one layer to another, such as wrapping
    TCP stream errors into TLS errors.
    
    Example:
    {[
      let tcp_writer = Tcp_stream.to_writer stream in
      let tls_writer = IO.Writer.map_err tcp_writer
        ~fn:(fun err -> Tls_error (Transport_error err))
    ]} *)
val flush: ('dst, 'err) t -> (unit, 'err) result

(** [flush writer] ensures all buffered data is written.

    For unbuffered destinations this is typically a no-op. For buffered
    destinations, this forces pending data to be written.

    Example:
    {[
      let* () = IO.write_all writer ~buf:"Important data" in
      let* () = IO.flush writer in
      (* Now we know the data has been written *)
      Ok ()
    ]} *)
