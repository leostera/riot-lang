type 'v bound =
  | Unbounded
  | Included of 'v
  | Excluded of 'v
type 'v range = 'v bound * 'v bound
type 'v t = 'v range list
val empty : 'v t

val full : 'v t

val singleton : 'v -> 'v t

val higher_than : 'v -> 'v t

val strictly_higher_than : 'v -> 'v t

val lower_than : 'v -> 'v t

val strictly_lower_than : 'v -> 'v t

val between : 'v -> 'v -> 'v t

val is_empty : 'v t -> bool

val complement : compare_v:('v -> 'v -> int) -> 'v t -> 'v t

val intersection : compare_v:('v -> 'v -> int) -> 'v t -> 'v t -> 'v t

val union : compare_v:('v -> 'v -> int) -> 'v t -> 'v t -> 'v t

val contains : compare_v:('v -> 'v -> int) -> 'v t -> 'v -> bool

val is_disjoint : compare_v:('v -> 'v -> int) -> 'v t -> 'v t -> bool

val subset_of : compare_v:('v -> 'v -> int) -> 'v t -> 'v t -> bool
