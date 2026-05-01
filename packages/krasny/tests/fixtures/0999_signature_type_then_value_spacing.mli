type t
val make: unit -> t

module type S = sig type inner val render: inner -> string end
