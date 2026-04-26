type t
val make:
  ?percent:float ->
  ?full_char:string ->
  ?trail_char:string ->
  ?empty_char:string ->
  ?color:[ | `Plain of Style.color | `Gradient of Style.color * Style.color] ->
  ?show_percentage:bool ->
  width:int ->
  unit ->
  t

val is_finished: t -> bool

val reset: t -> t

val set_progress: t -> progress:float -> t

val increment: t -> delta:float -> t

val view: t -> Gooey.Element.t
