(lang dune 3.14)

(name serde_toml)

(generate_opam_files true)

(source
 (github serde-ml/toml))

(authors "Rashid Almheiri <r.muhairi@pm.me>")

(maintainers "Rashid Almheiri <r.muhairi@pm.me>")

(license MIT)

(package
 (name serde_toml)
 (synopsis "TOML format support for Serde")
 (depends
  dune
  (ocaml
   (>= "5.1.1"))
  (serde
   (= :version))
  ;; Testing
  (serde_derive
   (and
    :with-test
    (= :version)))
  (spices :with-test)
  (qcheck :with-test))
 (tags
  (serde serialize deserialize toml)))
