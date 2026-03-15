(lang dune 3.0)

(name meridian)

(generate_opam_files true)

(package
 (name meridian)
 (depends
  (ocaml (>= 4.14))
  (alcotest (and :with-test (>= 1.0)))))
