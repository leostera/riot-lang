(* utils/config.generated.ml.  Generated from config.generated.ml.in by configure. *)
#2 "utils/config.generated.ml.in"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This file is included in config_main.ml during the build rather
   than compiled on its own *)

let bindir = {|/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/bin|}

let standard_library_default = {|/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/lib/ocaml|}

let ccomp_type = {|cc|}
let c_compiler = {|clang --target=x86_64-linux-gnu -fuse-ld=lld --sysroot=/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/sysroot|}
let c_output_obj = {|-o |}
let c_has_debug_prefix_map = false
let as_has_debug_prefix_map = false
let bytecode_cflags = {|-O2 -fno-strict-aliasing -fwrapv -fPIC -Qunused-arguments -pthread |}
let bytecode_cppflags = {| -D_FILE_OFFSET_BITS=64 |}
let native_cflags = {|-O2 -fno-strict-aliasing -fwrapv  -Qunused-arguments -pthread |}
let native_cppflags = {| -D_FILE_OFFSET_BITS=64 |}

let bytecomp_c_libraries = {|    -lpthread|}
(* bytecomp_c_compiler and native_c_compiler have been supported for a
   long time and are retained for backwards compatibility.
   For programs that don't need compatibility with older OCaml releases
   the recommended approach is to use the constituent variables
   c_compiler, {bytecode,native}_c[pp]flags etc. directly.
*)
let bytecomp_c_compiler =
  c_compiler ^ " " ^ bytecode_cflags ^ " " ^ bytecode_cppflags
let native_c_compiler =
  c_compiler ^ " " ^ native_cflags ^ " " ^ native_cppflags
let native_c_libraries = {|   -lpthread|}
let native_ldflags = {||}
let native_pack_linker = {|ld -r -o |}
let default_rpath = {|-Wl,-rpath,|}
let mksharedlibrpath = {|-Wl,-rpath,|}
let ar = {|llvm-ar|}
let supports_shared_libraries = true
let native_dynlink = true
let mkdll = {|clang --target=x86_64-linux-gnu -fuse-ld=lld --sysroot=/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/sysroot -shared |}
let mkexe = {|clang --target=x86_64-linux-gnu -fuse-ld=lld --sysroot=/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/sysroot  -Wl,-E |}
let mkmaindll = {|clang --target=x86_64-linux-gnu -fuse-ld=lld --sysroot=/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/sysroot -shared |}

let flambda = false
let with_flambda_invariants = false
let with_cmm_invariants = false
let windows_unicode = 0 != 0

let flat_float_array = true

let function_sections = true
let afl_instrument = false

let native_compiler = true

let architecture = {|amd64|}
let model = {|default|}
let system = {|linux|}

let asm = {|clang --target=x86_64-linux-gnu -fuse-ld=lld --sysroot=/Users/ostera/Developer/github.com/riot-ml/riot/ocaml/cross-compiler/x86_64-linux-gnu/sysroot -c|}
let asm_cfi_supported = true
let with_frame_pointers = false
let reserved_header_bits = 0

let ext_exe = {||}
let ext_obj = "." ^ {|o|}
let ext_asm = "." ^ {|s|}
let ext_lib = "." ^ {|a|}
let ext_dll = "." ^ {|so|}

let host = {|x86_64-pc-linux-gnu|}
let target = {|x86_64-pc-linux-gnu|}

let systhread_supported = true

let flexdll_dirs = []

let ar_supports_response_files = true

let tsan = false
