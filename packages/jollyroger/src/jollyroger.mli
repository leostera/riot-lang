(**
   Terminal design-system primitives for Riot command output.

   Jolly Roger centralizes semantic colors, status labels, and small layout
   helpers so Riot command reporters can share one visual language without
   duplicating ANSI escape sequences or spacing rules.
*)
module Palette: sig
  (** Reading surface: #FFF8ED. *)
  val paper: Tty.Color.t

  (** Recessed surface: #F5ECDC. *)
  val paper_2: Tty.Color.t

  (** Text and structure: #151317. *)
  val ink: Tty.Color.t

  (** Code gravity and terminal surface: #0E0D10. *)
  val coal: Tty.Color.t

  (** Identity and action red: #EF233C. *)
  val riot: Tty.Color.t

  (** Success mint: #24C08D. *)
  val mint: Tty.Color.t

  (** Warning amber: #F0B429. *)
  val amber: Tty.Color.t

  (** Reference blue: #2777FF. *)
  val blue: Tty.Color.t

  (** Brand red used for primary emphasis. Alias for [riot]. *)
  val brand: Tty.Color.t

  (** Hover red: #C91F38. *)
  val brand_hover: Tty.Color.t

  (** Active red: #9F172C. *)
  val brand_active: Tty.Color.t

  (** Slightly darker brand red for text links and labels. Alias for [brand_active]. *)
  val brand_text: Tty.Color.t

  (** Default reading background. Alias for [paper]. *)
  val background: Tty.Color.t

  (** Soft contrast background. Alias for [paper_2]. *)
  val background_subtle: Tty.Color.t

  (** Terminal surface. Alias for [coal]. *)
  val terminal: Tty.Color.t

  (** Body text. Alias for [ink]. *)
  val text: Tty.Color.t

  (** Strong text. Alias for [coal]. *)
  val text_strong: Tty.Color.t

  (** Muted text: #5B5462. *)
  val text_muted: Tty.Color.t

  (** Subtle text: #9AA0AA. *)
  val text_subtle: Tty.Color.t

  (** Inverse text: #FFFDF7. *)
  val text_inverse: Tty.Color.t

  (** Syntax text for dark code surfaces: #E6E2D6. *)
  val syntax_text: Tty.Color.t

  (** Default terminal text color for dark code surfaces. Alias for [syntax_text]. *)
  val terminal_text: Tty.Color.t

  (** Muted terminal text for secondary details. Alias for [text_subtle]. *)
  val muted: Tty.Color.t

  (** Successful operation color. Alias for [mint]. *)
  val success: Tty.Color.t

  (** Warning color. Alias for [amber]. *)
  val warning: Tty.Color.t

  (** Error color. Alias for [riot]. *)
  val danger: Tty.Color.t

  (** Informational color. Alias for [blue]. *)
  val info: Tty.Color.t

  (** Syntax string color. *)
  val syntax_string: Tty.Color.t

  (** Syntax number color. *)
  val syntax_number: Tty.Color.t

  (** Syntax type color. *)
  val syntax_type: Tty.Color.t

  (** Syntax comment color. *)
  val syntax_comment: Tty.Color.t

  module LightMode: sig
    (** Action color lifted for paper/light surfaces. *)
    val action: Tty.Color.t

    (** Success color lifted for paper/light surfaces. *)
    val success: Tty.Color.t

    (** Warning color lifted for paper/light surfaces. *)
    val warning: Tty.Color.t

    (** Danger color lifted for paper/light surfaces. *)
    val danger: Tty.Color.t

    (** Reference color lifted for paper/light surfaces. *)
    val reference: Tty.Color.t

    (** Muted color for secondary text on paper/light surfaces. *)
    val muted: Tty.Color.t
  end

  module DarkMode: sig
    (** Action color lifted for coal/dark surfaces. *)
    val action: Tty.Color.t

    (** Success color lifted for coal/dark surfaces. *)
    val success: Tty.Color.t

    (** Warning color lifted for coal/dark surfaces. *)
    val warning: Tty.Color.t

    (** Danger color lifted for coal/dark surfaces. *)
    val danger: Tty.Color.t

    (** Reference color lifted for coal/dark surfaces. *)
    val reference: Tty.Color.t

    (** Muted color for secondary text on coal/dark surfaces. *)
    val muted: Tty.Color.t
  end
end

module Terminal: sig
  type color_mode =
    | LightMode
    | DarkMode
  type t
  type status =
    | Plan
    | Running
    | Building
    | Success
    | Warning
    | Error
    | Built
    | Cached
    | Skipped

  val make: ?profile:Tty.Profile.t -> ?color:bool -> ?color_mode:color_mode -> unit -> t

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

  (** Plain-text-safe status label such as [built] or [error]. *)
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
