(* ocamltest/ocamltest_config.ml.  Generated from ocamltest_config.ml.in by configure. *)
#2 "ocamltest/ocamltest_config.ml.in"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The configuration module for ocamltest *)

let arch = {|amd64|}

let afl_instrument = false

let asm = {|clang --target=x86_64-linux-gnu -fuse-ld=lld --sysroot=/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/sysroot -c|}

let cpp = {|clang --target=x86_64-linux-gnu -fuse-ld=lld --sysroot=/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/sysroot -E -P|}

let cppflags = {| -D_FILE_OFFSET_BITS=64|}

let cc = {|clang --target=x86_64-linux-gnu -fuse-ld=lld --sysroot=/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/sysroot|}

let cflags = {|-O2 -fno-strict-aliasing -fwrapv|}

let ccomptype = {|cc|}

let diff = {||}
let diff_flags = {||}

let shared_libraries = true

let libunix = Some true

let systhreads = true

let str = true

let objext = {|o|}

let libext = {|a|}

let asmext = {|s|}

let system = {|linux|}

let ocamlsrcdir = {|/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/build|}

let flambda = false

let ocamlc_default_flags = ""
let ocamlopt_default_flags = ""

let flat_float_array = true

let ocamldoc = false

let ocamldebug = false

let native_compiler = true

let native_dynlink = true

let shared_library_cflags = {|-fPIC|}

let sharedobjext = {|so|}

let csc = {||}

let csc_flags = {||}

let exe = {||}

let mkdll = {|clang --target=x86_64-linux-gnu -fuse-ld=lld --sysroot=/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/sysroot -shared |}
let mkexe = {|clang --target=x86_64-linux-gnu -fuse-ld=lld --sysroot=/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/sysroot  -Wl,-E |}

let bytecc_libs = {|    -lpthread|}

let nativecc_libs = {|   -lpthread|}

let windows_unicode = 0 != 0

let function_sections = true

let instrumented_runtime = false

let frame_pointers = false

let tsan = false
