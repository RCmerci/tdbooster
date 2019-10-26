(lang dune 1.11)
(name tdbooster)

(generate_opam_files true)

(license MIT)
(maintainers "rcmerci <rcmerci@gmail.com>")
(authors "rcmerci <rcmerci@gmail.com>")
(source (github RCmerci/tdbooster))


(package
 (name tdbooster)
 (depends
  (ocaml (>= 4.07))
  core
  ppx_deriving
  ppx_deriving_yojson
  ppx_here
  ppx_inline_test
  ppx_jane
  yojson)
  (synopsis "Tdbooster"))
