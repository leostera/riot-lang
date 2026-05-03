open Std

(**
   Reentrant HTTP/2 frame parser over `IO.Reader`.

   This parser is designed for streaming, non-blocking I/O:
   - Uses `IO.Reader.t` instead of bytes directly
   - Maintains state between calls
   - Returns `Need_more` when data is incomplete
   - Never blocks or allocates large buffers upfront
*)

(** Parser configuration. *)
type config = {
  (** Maximum frame size. Default: `16384`. *)
  max_frame_size: int;
}

(** Default configuration. *)
val default_config: config

(** Opaque parser state that tracks position in frame parsing. *)
type state

(** Create a new parser state. *)
val create: ?config:config -> unit -> state

(** Parse errors. *)
type parse_error =
  (** Reader returned an IO error while reading frame bytes. *)
  | ReadFailed of IO.error
  | FrameParseFailed of Parser.error

val parse_error_to_string: parse_error -> string

(** Parse result. *)
type parse_result =
  (** Successfully parsed a complete frame. *)
  | Frame of Frame.t
  (** Need more data. Call again with more bytes in reader. *)
  | Need_more
  | Error of parse_error

(**
   Parse the next frame from the reader.

   This function is reentrant; you can call it multiple times as data arrives.
   The parser state tracks where it left off.

   Example usage:
   ```ocaml
     let parser = Parser_reader.create () in
     let reader = IO.Reader.from_source source stream in

     let rec read_frames () =
       match Parser_reader.parse parser reader with
       | Frame frame ->
           (* Process frame *)
           handle_frame frame;
           read_frames ()  (* Parse next frame *)
       | Need_more ->
           (* Wait for more data, then call again *)
           yield ();
           read_frames ()
       | Error e ->
           (* Handle error *)
           handle_error e
   ```
*)
val parse: state -> IO.Reader.t -> parse_result

(** Reset parser state to initial state for connection reuse. *)
val reset: state -> unit

(** Return bytes buffered in parser state for debugging. *)
val buffered_bytes: state -> int
