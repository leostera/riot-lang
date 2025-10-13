(** {1 Iovec - IO Vectors for efficient scatter/gather operations}

    IO vectors allow reading into or writing from multiple buffers in a single
    system call, which is more efficient than multiple individual operations.

    This is a thin wrapper around {!Kernel.Async.Iovec} to keep the API clean.

    {2 Example}

    Reading into multiple buffers:
    {[
      let header_buf = Bytes.create 128 in
      let body_buf = Bytes.create 4096 in
      let iov = Iovec.create ~count:2 ~size:4224 () in
      
      match IO.read_vectored reader iov with
      | Ok n -> Printf.printf "Read %d bytes total\n" n
      | Error e -> handle_error e
    ]}

    Creating from existing buffers:
    {[
      let buf1 = Bytes.of_string "Hello, " in
      let buf2 = Bytes.of_string "world!" in
      let iov = Iovec.of_bytes buf1 in
      (* Use with write_owned_vectored *)
    ]}
*)

type iov = Kernel.Async.Iovec.iov = { 
  ba : bytes;    (** The buffer *)
  off : int;     (** Offset into the buffer *)
  len : int;     (** Length to read/write *)
}
(** A single buffer descriptor in an IO vector.
    
    - [ba] is the actual byte buffer
    - [off] is where to start reading/writing in the buffer
    - [len] is how many bytes to use from [off]
*)

type t = Kernel.Async.Iovec.t
(** An array of buffer descriptors for scatter/gather operations *)

val with_capacity : int -> t
(** [with_capacity size] creates an IO vector with a single buffer of [size] bytes.
    
    Example:
    {[
      let iov = Iovec.with_capacity 4096 in
      (* Use for reading *)
    ]}
*)

val create : ?count:int -> size:int -> unit -> t
(** [create ?count ~size ()] creates an IO vector with [count] buffers.
    
    The [size] is distributed equally among all buffers. Default [count] is 1.
    
    Example:
    {[
      (* Two 2KB buffers *)
      let iov = Iovec.create ~count:2 ~size:4096 () in
      
      (* Single 1KB buffer *)
      let iov = Iovec.create ~size:1024 () in
    ]}
    
    @param count Number of buffers (default: 1, must be > 0)
    @param size Total size in bytes (must be > 0)
*)

val sub : ?pos:int -> len:int -> t -> t
(** [sub ?pos ~len iov] extracts a sub-vector starting at [pos] with length [len].
    
    This is useful after a partial read/write to get the remaining buffers.
    
    Example:
    {[
      let iov = Iovec.create ~count:3 ~size:3000 () in
      match IO.write_owned_vectored writer ~bufs:iov with
      | Ok 1500 -> 
          (* Only wrote 1500 bytes, get remaining *)
          let remaining = Iovec.sub ~len:1500 iov in
          IO.write_owned_vectored writer ~bufs:remaining
      | Ok _ -> Ok ()
      | Error e -> Error e
    ]}
    
    @param pos Starting position (default: 0)
    @param len Number of bytes to include
*)

val length : t -> int
(** [length iov] returns the total number of bytes across all buffers.
    
    Example:
    {[
      let iov = Iovec.create ~count:3 ~size:3000 () in
      assert (Iovec.length iov = 3000)
    ]}
*)

val iter : t -> (iov -> unit) -> unit
(** [iter iov f] calls [f] on each buffer descriptor in [iov].
    
    Example:
    {[
      Iovec.iter iov (fun { ba; off; len } ->
        Printf.printf "Buffer: off=%d len=%d\n" off len
      )
    ]}
*)

val of_bytes : bytes -> t
(** [of_bytes buf] creates an IO vector from a single byte buffer.
    
    Example:
    {[
      let buf = Bytes.of_string "Hello, world!" in
      let iov = Iovec.of_bytes buf in
      IO.write_owned_vectored writer ~bufs:iov
    ]}
*)

val from_string : string -> t
(** [from_string s] creates an IO vector from a string.
    
    The string is converted to bytes first.
    
    Example:
    {[
      let iov = Iovec.from_string "Response data" in
      IO.write_owned_vectored writer ~bufs:iov
    ]}
*)

val from_buffer : Buffer.t -> t
(** [from_buffer buf] creates an IO vector from a Buffer.
    
    Example:
    {[
      let buf = Buffer.create 1024 in
      Buffer.add_string buf "Some data";
      let iov = Iovec.from_buffer buf in
      IO.write_owned_vectored writer ~bufs:iov
    ]}
*)

val into_string : t -> string
(** [into_string iov] converts all data in the IO vector to a string.
    
    This concatenates all buffers into a single string.
    
    Example:
    {[
      let iov = Iovec.create ~size:4096 () in
      match IO.read_vectored reader iov with
      | Ok n ->
          let data = Iovec.into_string iov in
          process_data data
      | Error e -> Error e
    ]}
*)
