type quick_stat = { minor_collections: int; major_collections: int; compactions: int }

let quick_stat = fun () ->
  let stats = Caml_runtime.gc_quick_stat () in { minor_collections = stats.minor_collections; major_collections = stats.major_collections; compactions = stats.compactions }

let major = fun () -> Caml_runtime.gc_major ()

let full_major = fun () -> Caml_runtime.gc_full_major ()

let compact = fun () -> Caml_runtime.gc_compact ()
