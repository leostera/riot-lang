(** TLS engine using OpenSSL BIO pairs
    
    This module provides a transport-agnostic TLS implementation using OpenSSL's
    BIO pair mechanism. The TLS engine is decoupled from file descriptors,
    allowing it to work over any byte stream (TCP, Unix sockets, pipes, memory). *)

open Global0
open IO

type engine
(** Opaque TLS engine with BIO pairs *)

(** {2 Initialization} *)

val init : unit -> unit
(** Initialize OpenSSL library. Should be called once at startup. *)

val is_available : unit -> bool
(** Check if TLS is available on this platform. *)

val version : unit -> string
(** Get OpenSSL version string. *)

(** {2 Engine Creation} *)

val create_client_engine : hostname:string -> engine
(** Create a TLS client engine.
    
    @param hostname Server hostname for SNI and certificate verification.
    @raise Failure if engine creation fails. *)

val create_server_engine : cert_file:string -> key_file:string -> engine
(** Create a TLS server engine.
    
    @param cert_file Path to server certificate (PEM format)
    @param key_file Path to server private key (PEM format)
    @raise Failure if engine creation fails or certificates can't be loaded. *)

(** {2 Data Pumping} 

    These functions move data between the network and the TLS engine.
    All operations are non-blocking (memory-only). *)

val pump_encrypted_in : engine -> bytes -> pos:int -> len:int -> int
(** [pump_encrypted_in engine buf ~pos ~len] writes encrypted data from network
    into the TLS engine.
    
    This data will be decrypted by the engine and made available via
    [read_decrypted].
    
    @return Number of bytes consumed (always succeeds, never blocks). *)

val read_encrypted_out : engine -> bytes -> int
(** [read_encrypted_out engine buf] reads encrypted data from the TLS engine
    that needs to be sent to the network.
    
    This is data that the engine has encrypted and queued for transmission.
    
    @return Number of bytes read (0 if nothing pending, never blocks). *)

(** {2 Application I/O}

    These functions handle plaintext application data. They may return
    special codes indicating the engine needs network I/O. *)

type read_result =
  | Read of int                (** Successfully read n bytes *)
  | Need_network_read          (** Engine needs encrypted data from network *)
  | Need_network_write         (** Engine needs to send encrypted data to network *)
  | Eof                        (** Clean TLS shutdown *)

val read_decrypted : engine -> bytes -> pos:int -> len:int -> read_result
(** [read_decrypted engine buf ~pos ~len] reads plaintext application data
    from the TLS engine.
    
    The engine decrypts data that was pumped in via [pump_encrypted_in].
    
    Returns:
    - [Read n] if n bytes were read successfully
    - [Need_network_read] if the engine needs more encrypted input
    - [Need_network_write] if the engine needs to flush encrypted output first
    - [Eof] on clean TLS shutdown
    
    This never blocks - it only manipulates memory buffers. *)

type write_result =
  | Written of int             (** Successfully wrote n bytes *)
  | Need_network_read          (** Engine needs encrypted data (renegotiation) *)
  | Need_network_write         (** Engine needs to send encrypted data *)

val write_plaintext : engine -> bytes -> pos:int -> len:int -> write_result
(** [write_plaintext engine buf ~pos ~len] writes plaintext application data
    to the TLS engine.
    
    The engine encrypts this data and queues it for transmission.
    Use [read_encrypted_out] to retrieve the encrypted bytes.
    
    Returns:
    - [Written n] if n bytes were written successfully
    - [Need_network_read] if the engine needs more encrypted input first
    - [Need_network_write] if the engine needs to flush encrypted output
    
    This never blocks - it only manipulates memory buffers. *)

(** {2 TLS State} *)

type handshake_result =
  | Handshake_done
  | Need_network_read
  | Need_network_write

val do_handshake : engine -> handshake_result
(** Explicitly trigger the TLS handshake.
    
    For clients, this must be called to initiate the handshake.
    Returns the next action needed to advance the handshake. *)

val handshake_complete : engine -> bool
(** Check if the TLS handshake is complete.
    
    During handshake, [read_decrypted] and [write_plaintext] will return
    [Need_network_read]/[Need_network_write] to drive the handshake forward. *)

val alpn_protocol : engine -> string option
(** Get the negotiated ALPN protocol (e.g., "h2", "http/1.1").
    
    Returns [None] if no ALPN was negotiated. *)
