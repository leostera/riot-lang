type quick_stat = {
  minor_collections: int;
  major_collections: int;
  compactions: int;
}
val quick_stat: unit -> quick_stat

val major: unit -> unit

val full_major: unit -> unit

val compact: unit -> unit
