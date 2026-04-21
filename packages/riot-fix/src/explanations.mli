val all: unit -> Explanation.t list

val explain: Rule_id.t -> Explanation.t option

val format: Explanation.t -> string
