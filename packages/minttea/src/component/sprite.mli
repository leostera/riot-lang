type t

val make :
  ?starting_frame:int -> ?loop:bool -> fps:Fps.t -> string array -> t

val update : ?now:Std.Time.Instant.t -> t -> t
val view : t -> string
val current_frame_index : t -> int
