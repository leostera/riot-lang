(lang dune 3.17)
(name riot)
(version dev)
(formatting disabled)
(generate_opam_files false)

(source
 (github riot-ml/riot))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")
(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")
(license "MIT")

(package
 (name riot)
 (allow_empty)
 (synopsis "an actor-model multi-core scheduler for ocaml 5")
 (description "riot is an erlang-style actor-model multi-core scheduler for ocaml 5. it provides lightweight processes, message-passing, and fault-tolerance primitives.")
 (depends ocaml))

(package
 (name gluon)
 (synopsis "Minimal kqueue-based I/O event notification for macOS")
 (description "A clean, minimal interface for I/O event notification using kqueue on macOS")
 (depends ocaml))

(package
 (name miniriot)
 (synopsis "Minimal single-core actor runtime for build systems")
 (description "A stripped-down version of Riot focusing on single-core process management and message passing for use in build systems")
 (depends ocaml gluon))

(package
 (name minitusk)
 (synopsis "Task runner built on miniriot")
 (description "A task runner and build system built on the miniriot actor framework")
 (depends ocaml miniriot))
