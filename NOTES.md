# Macro ideas
some use cases for the macro rfd:
* include!(file)  -- basically drops the contents into the current file
* include_string!(file) -- puts the contents as a string
* include_bytes!(file) -- puts the contents as a bytes

* env!(var) -- string fetches env var at compile-time, and its compile-time error if it doesn't exist
* env!(var, default) -- same as above but returns default if not present

* quote!(code) -- macro quotation :) 

* dbg!(expr) -- hard one but basically print a debug representation of the value

* [@derive(...)] -- deriving macros ofc

* [@serde(...)] -- serde macros

* a set of macros for logging provided by std like info! debug! warn! error! trace! 

* todo!() macro -- panics saying that this is yet to be done
* unreachable!() -- panics saying this path should never have been executed
* panic!(msg) -- panics

* format!("...") -- returna a formatted String

* [@lint_rule] -- to declare a linting rule in a single function like

  [@lint_rule(id="e0001", hint="do not use stdlib!", explain=".....")]
  let no_stdlib tree = ....

  and voila that does all the plumbing for you

* package_name! module_name! function_name! loc! file! - and other context-level things that can be injected

### Parked Release/Toolchain Follow-Up

- Status update after enabling the rebased OCaml 5.5 relocatable toolchain support:
  - the original relocatability regression is fixed
  - cross-toolchain packaging is also fixed now; published artifacts preserve the full installed layout
- Verified on Apple Silicon macOS:
  - `./scripts/release/ocaml.sh aarch64-apple-darwin-x-aarch64-unknown-linux-gnu` now completes successfully when run with uploads disabled
  - `vendor/ocaml/cross/aarch64-apple-darwin/bin/ocamlc -config` now reports `standard_library_relative: ../lib/ocaml`
  - `vendor/ocaml/runtime/build_config.h` now contains `#define OCAML_STDLIB_DIR "../lib/ocaml"`
  - `vendor/ocaml/Makefile.build_config` now has `TARGET_LIBDIR=../lib/ocaml` and `TARGET_LIBDIR_IS_RELATIVE=true`
  - the installed native and cross compilers can compile a trivial module without `OCAMLLIB`
  - packaged native and cross tarballs both survive extraction into a different absolute path under `env -i`
  - the packaged Linux cross tarball now includes `gcc/`, and relocated `ocamlopt` can emit an ELF aarch64 binary while the bundled `aarch64-linux-gnu-gcc -print-sysroot` resolves inside the extracted toolchain
- Practical implication:
  - native host tarballs look relocatable and usable
  - cross tarballs now look relocatable and self-contained too
  - there is now a simple local packaging path for all built toolchains via `vendor/ocaml/cross/package-all.sh`
- Follow-up to do next:
  - revisit `./docker/build.sh`; the OCaml toolchain packaging path now looks good, but Docker itself has not been re-verified yet
  - if we want stronger regression coverage, wire `vendor/ocaml/cross/test-relocatable.sh` into CI for at least one native and one cross target
