(lang dune 3.17)
(using mdx 0.4)
(using melange 0.1)

(name stack)

(cram enable)


(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(package
 (name stack)
 (depends
   (alcotest (and :with-test (>= "1.7.0")))
   (bitstring (>= "4.1.0"))
   (crunch (>= "3.3.1"))
   (decompress (>= "1.5.3"))
   (digestif (>= "1.1.4"))
   (http (>= "6.0.0~beta2"))
   (httpaf (>= "0.7.1"))
   (magic-mime (>= "1.3.1"))
   (magic-mime (>= "1.3.1"))
   (mdx (and :with-test (>= "2.3.1")))
   (mirage-crypto (>= "0.11.2"))
   (mirage-crypto-rng (>= "0.11.2"))
   (mtime (>= "2.0.0"))
   (ocaml (>= "5.2"))
   (odoc (and :with-doc (>= "2.2.2")))
   (ppx_bitstring (>= "4.1.0"))
   (ppx_deriving (>= "5.2.1"))
   (ppx_inline_test (>= "v0.16.0"))
   (ppxlib (>= 0.31.0))
   (ptime (>= "1.1.0"))
   (qcheck :with-test)
   (sedlex (>= "3.2"))
   (tls (>= "1.0.0"))
   (uri (>= "4.4.0"))
   (uuidm (>= "0.9.8"))
   (uutf (>= "1.0.3"))
   (x509 (>= "0.16.5"))
   (yojson (>= "2.1.0"))
   melange
   ))
