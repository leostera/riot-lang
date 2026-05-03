open Std

(** MIME parsing and rendering helpers. *)

(** MIME content type. *)
type content_type = {
  (** Top-level media type, such as `"text"` or `"multipart"`. *)
  media_type: string;
  (** Media subtype, such as `"plain"` or `"mixed"`. *)
  subtype: string;
  (** Additional content-type parameters. *)
  parameters: (string * string) List.t;
}
(** MIME content disposition. *)
type content_disposition =
  | Inline of {
      filename: string Option.t;
    }
  | Attachment of {
      filename: string Option.t;
    }
(** Content-transfer encoding. *)
type encoding =
  | SevenBit
  | EightBit
  | Binary
  | QuotedPrintable
  | Base64
  | Other of string
(** Parsed MIME header. *)
type header =
  | ContentType of content_type
  | ContentDisposition of content_disposition
  | ContentTransferEncoding of encoding
  | ContentId of string
  | ContentDescription of string
  | Other of string * string
(** One MIME body part. *)
type part = {
  (** Parsed part headers. *)
  headers: header List.t;
  (** Raw part content. *)
  content: string;
}
(** Parsed MIME entity. *)
type t =
  | SinglePart of part
  | MultiPart of {
      boundary: string;
      parts: t List.t;
    }

(**
   Parse a MIME entity from headers and body text.

   Use this when you already have the raw message split into headers and body,
   for example after parsing an email message or multipart HTTP payload.
*)
val parse: headers:(string * string) List.t -> body:string -> (t, string) Result.t

(**
   Return all attachment parts reachable in the MIME tree.

   Example:
   ```ocaml
   let files = Mime.attachments message
   ```
*)
val attachments: t -> part List.t

(** Return `true` if the part should be treated as an attachment. *)
val is_attachment: part -> bool

(** Return the filename associated with the part, if one exists. *)
val get_filename: part -> string Option.t

(** Return the parsed content type for the part, if present. *)
val get_content_type: part -> content_type Option.t

(** Return the transfer encoding for the part, if present. *)
val get_encoding: part -> encoding Option.t

(**
   Decode the part content using its transfer encoding.

   Use this when you need the payload bytes or text rather than the raw stored
   body representation.
*)
val get_decoded_content: part -> (string, string) Result.t

(** Find a header by name in a parsed header list. *)
val find_header: string -> header List.t -> header Option.t
