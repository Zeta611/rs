(lang dune 3.4)
(using menhir 2.0)

(name rs)

(generate_opam_files true)

(source
 (github Zeta611/rs))

(authors "Jay Lee")

(maintainers "Jay Lee")

(license LICENSE)

(package
 (name rs)
 (synopsis "The RS language")
 (description "The RS language, the target language of a class of ReScript static analyses.")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
