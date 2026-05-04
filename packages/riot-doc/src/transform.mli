open Std

val from_interface_source:
  lookup:Source.lookup ->
  ?path:string list ->
  ?docstring:string ->
  Source.interface_source ->
  (Doctree.module_doc, string) result
