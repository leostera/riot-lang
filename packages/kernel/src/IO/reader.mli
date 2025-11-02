(** {1 Reader}

    Generic abstraction for readable data sources.

    Reader provides a uniform interface for reading from any source (files,
    sockets, in-memory buffers, etc.) through a first-class module wrapper. This
    allows code to be polymorphic over the source of data and error types.

    {2 Design}

    The Reader abstraction uses first-class modules to provide type erasure
    while maintaining type safety. Sources implement the [Read] signature, then
    get wrapped in a [Reader.t] for uniform handling.

    The Reader is parametrized by both the source type ['src] and error type
    ['err], allowing each source to define its own error types without forcing a
    common error hierarchy.

    {2 Example}

    Creating a reader from a TCP stream:
    {[
      let stream = Net.TcpStream.connect addr in
      let reader = Net.TcpStream.to_reader stream in

      let buf = Bytes.create 4096 in
      match IO.read reader buf with
      | Ok n -> Printf.printf "Read %d bytes\n" n
      | Error `Closed -> Printf.printf "Connection closed\n"
      | Error _ -> Printf.printf "Read error\n"
    ]}

    Reading all data:
    {[
      let buffer = Buffer.create 1024 in
      match IO.Reader.read_to_end reader ~buf:buffer with
      | Ok total -> Printf.printf "Read %d total bytes\n" total
      | Error e -> handle_error e
    ]} *)

module type Read = sig
  (** Interface that readable sources must implement.

      Sources like TCP streams, files, or in-memory buffers implement this
      signature to become readable through the Reader abstraction. *)

  type t
  (** The type of the readable source *)

  type err
  (** The error type for this source *)

  val read : t -> ?timeout:int64 -> bytes -> (int, err) result
  (** [read src ?timeout buf] reads data from [src] into [buf].

      @param timeout
        Optional timeout in nanoseconds. If the read cannot complete within this
        time, returns a timeout error.
      @return Number of bytes read. Returns 0 on EOF.
      @raise Never raises - all errors are returned as [Error].

      The implementation should:
      - Read as much data as available, up to [Bytes.length buf]
      - Return the actual number of bytes read
      - Return 0 to indicate EOF
      - Return [Error] with appropriate error if the source is closed
      - Suspend the calling process if no data is available (for async sources)
  *)

  val read_vectored : t -> Iovec.t -> (int, err) result
  (** [read_vectored src iov] reads data into multiple buffers atomically.
      
      This is more efficient than multiple [read] calls when scattering data
      across multiple buffers. The implementation can use system calls like
      [readv] for optimal performance.
      
      @return Total number of bytes read across all buffers
      @see {!Iovec} for IO vector operations
  *)
end

type ('src, 'err) read = (module Read with type t = 'src and type err = 'err)
(** First-class module type for readable sources.

    This allows sources to be passed as values while maintaining their
    implementation. Type parameters:
    - ['src] is the concrete source type
    - ['err] is the error type for this source *)

type ('src, 'err) t
(** Reader wrapping a readable source.

    This is an existential type that hides the concrete source type while
    preserving its capabilities. Code working with [Reader.t] doesn't need to
    know the specific source implementation.

    Type parameters:
    - ['src] is the underlying source type (e.g., [Kernel.Net.Tcp_stream.t])
    - ['err] is the error type for read operations *)

val of_read_src : ('src, 'err) read -> 'src -> ('src, 'err) t
(** [of_read_src (module Read) src] creates a reader from a source.

    This is typically called by source modules (like [Net.TcpStream.to_reader])
    rather than by user code.

    Example implementation:
    {[
      let to_reader stream =
        let module Read = struct
          type t = Tcp_stream.t
          type err = Tcp_stream.error

          let read = Tcp_stream.read
          let read_vectored = Tcp_stream.read_vectored
        end in
        IO.Reader.of_read_src (module Read) stream
    ]} *)

val read : ('src, 'err) t -> ?timeout:int64 -> bytes -> (int, 'err) result
(** [read reader ?timeout buf] reads data from the reader into a buffer.

    @param timeout Optional timeout in nanoseconds
    @return Number of bytes read, or 0 on EOF

    This is the primary way to read data. Example:
    {[
      let buf = Bytes.create 4096 in
      match IO.read reader buf with
      | Ok 0 -> print_endline "EOF reached"
      | Ok n -> process_data (Bytes.sub buf 0 n)
      | Error e -> handle_error e
    ]} *)

val read_vectored : ('src, 'err) t -> Iovec.t -> (int, 'err) result
(** [read_vectored reader iov] reads data into multiple buffers.

    More efficient than multiple [read] calls when scattering data across
    buffers. Example:
    {[
      let header = Bytes.create 128 in
      let body = Bytes.create 4096 in
      let iov = Iovec.create ~count:2 ~size:4224 () in
      match IO.read_vectored reader iov with
      | Ok n -> Printf.printf "Read %d bytes total\n" n
      | Error e -> handle_error e
    ]} *)

val read_to_end : ('src, 'err) t -> buf:Buffer.t -> (int, 'err) result
(** [read_to_end reader ~buf] reads all available data until EOF.

    Repeatedly reads from [reader] until EOF (0 bytes returned) or error,
    accumulating all data in [buf].

    @param buf Buffer to accumulate data into
    @return Total number of bytes read

    Warning: This will read until EOF. For infinite streams (like network
    sockets that don't close), this will never return. Use with caution.

    Example:
    {[
      let buffer = Buffer.create 1024 in
      match IO.Reader.read_to_end reader ~buf:buffer with
      | Ok total ->
          let contents = Buffer.contents buffer in
          Printf.printf "Read %d bytes: %s\n" total contents
      | Error e -> handle_error e
    ]} *)

val empty : (unit, unit) t
(** [empty] is a reader that immediately returns 0 (EOF) on every read.

    This reader can never error, so its error type is [unit].

    Useful for testing or as a placeholder. Example:
    {[
      let reader = IO.Reader.empty in
      match IO.read reader (Bytes.create 100) with
      | Ok 0 -> print_endline "EOF as expected"
      | _ -> assert false
    ]} *)
