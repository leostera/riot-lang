type t

val make : float -> t
val of_int : int -> t
val of_float : float -> t
val tick : ?now:Std.Time.Instant.t -> t -> [ `frame | `skip ]
