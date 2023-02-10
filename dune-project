(lang dune 3.6)

(name feint)

(version 0.0.0)

(source
 (github feint-lang/feint-llvm))

(authors "Wyatt Baldwin")

(maintainers "Wyatt Baldwin")

(license MIT)

(documentation https://feint-lang.github.io/)

(generate_opam_files true)

(using menhir 2.0)

(package
 (name feint)
 (synopsis "FeInt")
 (description "FeInt")
 (depends ocaml dune linenoise))
