open Std

type content_type = {
  media_type : string;
  subtype : string;
  parameters : (string * string) List.t;
}
type content_disposition =
  | Inline of {
      filename : string Option.t;
    }
  | Attachment of {
      filename : string Option.t;
    }
type encoding =
  | SevenBit
  | EightBit
  | Binary
  | QuotedPrintable
  | Base64
  | Other of string
type header =
  | ContentType of content_type
  | ContentDisposition of content_disposition
  | ContentTransferEncoding of encoding
  | ContentId of string
  | ContentDescription of string
  | Other of string * string
type part = {
  headers : header List.t;
  content : string;
}
type t =
  | SinglePart of part
  | MultiPart of { boundary : string; parts : t List.t; }
val parse : headers:(string * string) List.t -> body:string -> (t, string) Result.t

val attachments : t -> part List.t

val is_attachment : part -> bool

val get_filename : part -> string Option.t

val get_content_type : part -> content_type Option.t

val get_encoding : part -> encoding Option.t

val get_decoded_content : part -> (string, string) Result.t

val find_header : string -> header List.t -> header Option.t
