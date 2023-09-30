(lang dune 3.10)

(name typestack)

(generate_opam_files true)

(source
 (github benmandrew/typestack))

(authors "Ben Andrew")

(maintainers "Ben Andrew")

(license MIT)

(package
 (name typestack)
 (synopsis "")
 (description "")
 (depends
  (ocaml (>= 4.14.0))
  (dune (>= 2.0.0))
  core
  (ppx_compare (>= v0.16.0))
  (sexplib (>= v0.16.0))
  (ppx_inline_test (and (>= v0.16.0) :with-test))))
