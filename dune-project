(lang dune 3.11)
(using mdx 0.4)

(name stack)

(generate_opam_files true)
(cram enable)

(source
 (github riot-ml/riot))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license MIT)

(package
 (name riot)
 (synopsis "An actor-model multi-core scheduler for OCaml 5")
 (description
  "Riot is an actor-model multi-core scheduler for OCaml 5. It brings Erlang-style concurrency to the language, where lighweight process communicate via message passing")
 (depends
  (mdx (and :with-test (>= "2.3.1")))
  (mirage-crypto (>= "0.11.2"))
  (mirage-crypto-rng (>= "0.11.2"))
  (mtime (>= "2.0.0"))
  (ocaml (>= "5.1"))
  (odoc (and :with-doc (>= "2.2.2")))
  (ptime (>= "1.1.0"))
  (tls (>= "1.0.0"))
  (uri (>= "4.4.0"))
  dune)
 (tags
  (multicore erlang actor "message-passing" processes)))

(package
  (name rio)
  (synopsis "Ergonomic, composable, efficient read/write streams")
  (description
    "Rio implements ergonomic, composable, efficient read/write streams that give you a uniform interface for reading from any source and writing to any sync.")
  (depends ocaml dune)
  (tags (multicore io streams read write)))

(package
 (name libc)
 (synopsis "Raw definitions and bindings to platforms system libraries")
 (description "`libc provides definitions (and in the future bindings) to easily interoperate with C code (or C-like code) on several platforms, and it does this with a target and architecture aware API.")
 (depends 
   (config (= "0.0.2"))
   (ocaml (>= "5.1.1"))
   dune)
 (tags
  (libc platform bindings "raw bindings")))

(package
 (name tty)
 (synopsis "A library for interacting with teletype and terminal emulators")
 (description "TTY is a library for directly interacting with teletypes and terminal emulators, including escape sequences, colors, and consuming stdin")
 (depends
   (ocaml (>= "5.1"))
   (uutf (>= "1.0.3")))
 (tags (terminal ansi tty teletype utf8)))

(package
 (name serde)
 (synopsis "A serialization framework for OCaml")
 (description "Serde is a serialization framework for OCaml that runs on the principle of maximum efficiency and user ergonomics while maintaining format flexibility.")
 (depends
   (ocaml (>= "5.1.1"))
   (qcheck :with-test)
   (rio (>= "0.0.8"))
   (spices :with-test)
   dune))

(package
 (name serde_derive)
 (synopsis "Derive-macros for the Serde serialization framework")
 (description "These macros help derive serializers and deserializers for your existing types and provide all the functionality you expect to plug in different data-formats.")
 (depends
   (ocaml (>= "5.1.1"))
   (ppx_deriving (>= "5.2.1"))
   (ppxlib (>= "0.28.0"))
   (serde (= :version))
   (serde_json (and :with-test (= :version)))
   dune))

(package
 (name serde_json)
 (synopsis "JSON format support for Serde")
 (depends
   (ocaml (>= "5.1.1"))
   (ppx_inline_test (>= "v0.16.0"))
   (qcheck :with-test)
   (rio (>= "0.0.8"))
   (serde (= :version))
   (serde_derive (and :with-test (= :version)))
   (spices :with-test)
   (yojson (>= "2.1.0"))
   dune))
   
(package
 (name trail)
 (synopsis "A minimalistic, composable web framework for building HTTP/WebSocket servers")
 (description "Trail is a minimalistic, composable framework for building HTTP/WebSocket servers for Riot, inspired by Elixir's Plug & WebSock. It provides you with a small set of abstractions for building _trails_ that can be assembled to handle a request. This includes a logger, a router, a socket endpoint, an interface to handling web socket frames, and more")
 (depends
  (alcotest (and :with-test (>= "1.7.0")))
  (atacama (>= "0.0.1"))
  (bytestring (>= "0.0.1"))
  (http (>= "6.0.0~beta2"))
  (magic-mime (>= "1.3.1"))
  (mdx (and :with-test (>= "2.3.1")))
  (ocaml (>= "5.1.0"))
  (ppx_bitstring (>= "4.1.0"))
  (qcheck (and :with-test (>= "0.21.3")))
  (riot (>= "0.0.1"))
  (uuidm (>= "0.9.8"))
  dune)
 (tags
  (composable "web framework" http websockets ws riot multicore)))

(package
 (name sidewinder)
 (synopsis "")
 (description "")
 (depends
  (mdx (and :with-test (>= "2.3.1")))
  (ocaml (>= "5.1.0"))
  (magic-mime (>= "1.3.1"))
  (riot (>= "0.0.1"))
  (trail (>= "0.0.1"))
  (crunch (>= "3.3.1"))
  (serde_derive (>= "0.0.1"))
  melange
  dune)
 (sites (share static))
 (tags
  (composable "web framework" http websockets ws riot multicore)))

(package
  (name gluon)
  (synopsis "A minimal, portable, and fast API on top of the operating-system's evented I/O API")
  (depends
    (bytestring (>= "0.0.1"))
    (config (>= "0.0.1"))
    (libc (>= "0.0.1"))
    (ocaml (>= "5.1"))
    (rio (>= "0.0.1"))
    (uri (>= "4.4.0"))
    dune)
  (tags (async io tcp udp multicore)))

(package
 (name nomad)
 (synopsis "A Web server for Trail applications")
 (description "Nomad is a web server for the Riot scheduler written entirely in OCaml and is built atop the Atacama connection pool. It aims to be an Application-layer for Trail, implementing: HTTP/1.x, HTTP/2, and WebSockets. It is written with a big focus on clarity.")
 (depends 
   (atacama (>= "0.0.1"))
   (bitstring (>= "4.1.0"))
   (decompress (>= "1.5.3"))
   (digestif (>= "1.1.4"))
   (httpaf (>= "0.7.1"))
   (ocaml (>= "5.1.0"))
   (ppx_bitstring (>= "4.1.0"))
   (riot (>= "0.0.1"))
   (telemetry (>= "0.0.1"))
   (trail (>= "0.0.1"))
   (uutf (>= "1.0.3")))
 (tags
  (riot trail http https websocket ws web server suri)))

(package
 (name scarab)
 (synopsis "Microbenchmarking framework for Riot")
 (description "")
 (depends
   (ocaml (>= "5.1.1"))
   (riot (>= "0.0.8"))
   (minttea (>= "0.0.2"))
   (mdx (and :with-test (>= "2.3.1")))
   dune
   )
 (tags (microbenchmark benchmark bench riot multicore tui minttea)))

(package
 (name colors)
 (synopsis "A pure OCaml library for manipulating colors across color spaces")
 (depends 
   (ocaml (>= "4.13.0"))
   (mdx (and :with-test (>= "2.3.1"))))
 (tags
  (color "color spaces" "color transformations" "color blending" luv xyz rgb "linear rgb" srgb uv "reference colors")))

(package
  (name config)
  (synopsis "Ergonomic, lightweight conditional compilation through attributes")
  (description "Config implements the [@@config] annotation, to conditionally compile modules. You can use a small boolean language to check for conditions including some built-in ones and environment variables.")
  (depends
    (ocaml (>= "5.1"))
    (ppxlib (>= 0.31.0))
    (spices (>= "0.0.2"))
    dune)
  (tags (conditional compilation target_os target_arch arch os system)))

(package
 (name castore)
 (synopsis "A portable CA Store with a global .crt and .pem files")
 (depends 
   (ocaml (>="5.1")) 
   (mdx (and :with-test (>= "2.3.1")))
   (x509 (>= "0.16.5"))
   dune)
 (tags (https tls cert crt pem ca "ca store")))

(package
  (name bytestring)
  (synopsis "Efficient, immutable, pattern-matchable, UTF friendly byte strings")
  (description "The Bytestring package implements efficient, immutable, pattern-matchable, UTF friendly byte strings backed up by a rope implementation with mutable iterator instances and a transient interface in the spirit of Clojure transient data.")
  (authors 
    "Leandro Ostera <leandro@abstractmachines.dev>"
    "Felipe Oliveira Carvalho <felipekde@gmail.com>")
  (depends
    (rio (= :version))
    (ocaml (>= "5.1.1"))
    (ppxlib (>= 0.31.0))
    (qcheck (and :with-test (>= "0.21.3")))
    (sedlex (>= "3.2"))
    (spices (>= "0.0.2"))
    dune
    )
  (tags (bytes strings "pattern matching" "binary strings" rope transient iterators utf8)))
