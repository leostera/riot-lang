type t

external create: unit -> t = "caml_ml_mutex_new"

external lock: t -> unit = "caml_ml_mutex_lock"

external unlock: t -> unit = "caml_ml_mutex_unlock"

external try_lock: t -> bool = "caml_ml_mutex_try_lock"
