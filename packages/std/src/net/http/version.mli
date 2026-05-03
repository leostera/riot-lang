(**
   HTTP protocol versions.

   HTTP protocol version representation supporting HTTP/0.9 through HTTP/3.

   ## Examples

   Working with versions:

   ```ocaml open Std.Net.Http

   let v1 = Version.Http11 in Version.to_string v1 (* "HTTP/1.1" *)

   match Version.of_string "HTTP/2" with | Ok v2 -> Version.compare v1 v2 (* <
   0, HTTP/1.1 < HTTP/2 *) | Error InvalidVersion -> ()

   Version.is_supported Version.Http11 (* true *) Version.is_supported
   Version.Http3 (* depends on implementation *) ```

   ## Supported Versions

   - HTTP/0.9 - Legacy, rarely used
   - HTTP/1.0 - Basic HTTP
   - HTTP/1.1 - Most common (default)
   - HTTP/2 - Binary protocol with multiplexing
   - HTTP/3 - QUIC-based protocol
*)
open Global

type t =
  | Http09
  | Http10
  | Http11
  | Http2
  | Http3
(** HTTP protocol versions from 0.9 to 3.0. *)
type error =
  | InvalidVersion

(**
   Parses an HTTP version string.

   ## Examples

   ```ocaml Version.of_string "HTTP/1.1" (* Ok Http11 *) Version.of_string
   "HTTP/2" (* Ok Http2 *) Version.of_string "HTTP/9.9" (* Error
   InvalidVersion *) ```

   Accepted formats:
   - "HTTP/0.9", "HTTP/1.0", "HTTP/1.1"
   - "HTTP/2", "HTTP/3"
*)
val of_string: string -> (t, error) Kernel.result

(** Parses an HTTP version from a borrowed slice without materializing a string first. *)
val from_slice: IO.IoVec.IoSlice.t -> (t, error) Kernel.result

(**
   Converts HTTP version to standard string representation.

   ## Examples

   ```ocaml Version.to_string Version.Http11 (* "HTTP/1.1" *) Version.to_string
   Version.Http2 (* "HTTP/2" *) ```
*)
val to_string: t -> string

(**
   Compares two HTTP versions by their version number.

   ## Examples

   ```ocaml Version.compare Version.Http10 Version.Http11 (* < 0 *)
   Version.compare Version.Http2 Version.Http11 (* > 0 *) ```
*)
val compare: t -> t -> Order.t

(**
   Checks if two HTTP versions are equal.

   ## Examples

   ```ocaml Version.equal Version.Http11 Version.Http11 (* true *) ```
*)
val equal: t -> t -> bool

(**
   Checks if the HTTP version is supported by this implementation.

   ## Examples

   ```ocaml Version.is_supported Version.Http11 (* typically true *) ```

   ## Note

   Support may vary by platform and build configuration.
*)
val is_supported: t -> bool
