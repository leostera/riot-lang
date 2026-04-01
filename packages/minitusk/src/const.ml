open Stdlib

let aliases_suffix = "__aliases"

let c_ext = ".c"

let cma_ext = ".cma"

let cmi_ext = ".cmi"

let cmo_ext = ".cmo"

let current_dir = "."

let h_ext = ".h"

let ml_ext = ".ml"

let ml_gen_extension = ".ml.gen"

let mli_ext = ".mli"

let src_dir = "src"

let native_dir = "native"
(** Get the home directory *)
let home_dir = Sys.getenv "HOME"
(** Get the host triple for the current platform *)
let get_host_triple = fun () -> "unknown-unknown-unknown"
(** Get the default OCaml version *)
let ocaml_version = "5.5.0-riot.1"
(** Get the toolchain directory for a given version and target *)
let get_toolchain_dir = fun ?(version = ocaml_version) ?(target = get_host_triple ()) () ->
  Filename.concat home_dir (Filename.concat ".tusk/toolchains" (Filename.concat version target))
(** Get the bin directory for a given toolchain *)
let get_toolchain_bin_dir = fun ?(version = ocaml_version) ?(target = get_host_triple ()) () ->
  Filename.concat (get_toolchain_dir ~version ~target ()) "bin"
(** Get the lib directory for a given toolchain *)
let get_toolchain_lib_dir = fun ?(version = ocaml_version) ?(target = get_host_triple ()) () ->
  Filename.concat (get_toolchain_dir ~version ~target ()) "lib/ocaml"
