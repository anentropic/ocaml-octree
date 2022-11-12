(lang dune 3.5)
(name oktree)
(version 0.1.0)

(generate_opam_files true)

(source (github anentropic/ocaml-oktree))
(license MIT)
(authors "Anentropic")
(maintainers "ego@anentropic.com")

(package
 (name oktree)
 (synopsis "OKtree: A simple octree implementation in OCaml")
 (description "A simple octree implementation in OCaml, intended to efficiently find nearest match for an arbitrary point among a set of points in 3D coordinate space.")
 (depends
  (ocaml (>= 4.10))
  (psq (>= 0.2))
  ; test deps
  (ppx_deriving_popper (and :with-test (>= 0.1.1)))
  (gg (and :with-test (>= 1.0)))

  ; benchmark deps (to be installed manually)
  ; - core_bench
  ; - core_unix
  ; - owl

  ; dev deps (to be installed manually)
  ; - ocaml-lsp-server
  ; - ocp-indent
  ; - ocamlformat
  ; - utop
 ))
