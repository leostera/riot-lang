type style =
  | Dots
  | Numerals
type t

val make:
  ?style:style ->
  ?page:int ->
  ?per_page:int ->
  ?total_pages:int ->
  ?active_dot:string ->
  ?inactive_dot:string ->
  ?numerals_format:(int -> int -> string) ->
  ?text_style:Style.t ->
  unit ->
  t

val set_total_pages: t -> total:int -> t * int

val get_slice_bounds: t -> int -> int * int

val items_on_page: t -> int -> int

val on_last_page: t -> bool

val on_first_page: t -> bool

val prev_page: t -> t

val next_page: t -> t

val update: t -> Event.t -> t

val view: t -> string
