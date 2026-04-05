open Std

val assets: (string * string) list

val render_index: Doctree.package_doc -> string

val render_module: Doctree.package_doc -> Doctree.module_doc -> string

val render_module_source: Doctree.package_doc -> Doctree.module_doc -> string
