(**
   Terminal design-system primitives for Riot command output.

   Jolly Roger centralizes semantic colors, status labels, and small layout
   helpers so Riot command reporters can share one visual language without
   duplicating ANSI escape sequences or spacing rules.
*)
module Palette: sig
  (** Brand red used for primary emphasis. *)
  val brand: Tty.Color.t

  (** Slightly darker brand red for text links and labels. *)
  val brand_text: Tty.Color.t

  (** Default terminal text color for dark code surfaces. *)
  val terminal_text: Tty.Color.t

  (** Muted terminal text for secondary details. *)
  val muted: Tty.Color.t

  (** Successful operation color. *)
  val success: Tty.Color.t

  (** Warning color. *)
  val warning: Tty.Color.t

  (** Error color. *)
  val danger: Tty.Color.t

  (** Informational color. *)
  val info: Tty.Color.t

  (** Syntax string color. *)
  val syntax_string: Tty.Color.t

  (** Syntax number color. *)
  val syntax_number: Tty.Color.t

  (** Syntax type color. *)
  val syntax_type: Tty.Color.t

  (** Syntax comment color. *)
  val syntax_comment: Tty.Color.t
end

module Terminal: sig
  type t
  type status =
    | Running
    | Success
    | Warning
    | Error
    | Built
    | Cached
    | Skipped

  val make: ?profile:Tty.Profile.t -> ?color:bool -> unit -> t

  (** Theme that never emits ANSI escapes. *)
  val plain: t

  (** Whether this theme emits ANSI escapes. *)
  val color_enabled: t -> bool

  (** Apply a [Tty.Style.t], converting colors to the terminal profile. *)
  val style: t -> Tty.Style.t -> string -> string

  (** Build a foreground style from a semantic color. *)
  val fg: Tty.Color.t -> Tty.Style.t

  (** Build a bold foreground style from a semantic color. *)
  val fg_bold: Tty.Color.t -> Tty.Style.t

  (** Muted secondary text. *)
  val muted: t -> string -> string

  (** Strong primary text. *)
  val strong: t -> string -> string

  (** Success text. *)
  val success: t -> string -> string

  (** Warning text. *)
  val warning: t -> string -> string

  (** Error text. *)
  val danger: t -> string -> string

  (** Informational text. *)
  val info: t -> string -> string

  (** Square, plain-text-safe status label such as [[built]] or [[error]]. *)
  val status_label: t -> status -> string

  (** Status label followed by a message. *)
  val status_line: t -> status -> string -> string
end

module Layout: sig
  (** Prefix [text] with [spaces] spaces. *)
  val indent: int -> string -> string

  (** ASCII bullet line, optionally indented. *)
  val bullet: ?indent:int -> string -> string

  (** A single [label: value] row, optionally indented. *)
  val field: ?indent:int -> label:string -> value:string -> unit -> string

  (** Aligned [label: value] rows, optionally indented. *)
  val fields: ?indent:int -> (string * string) list -> string list
end
