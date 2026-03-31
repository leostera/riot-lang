type t
val make: ?style:Style.t -> ?blink:bool -> ?fps:Fps.t -> unit -> t

val update: t -> Event.t -> t

val view: t -> text_style:Style.t -> string -> string

val focus: t -> t

val unfocus: t -> t

val disable_blink: t -> t

val enable_blink: t -> t
