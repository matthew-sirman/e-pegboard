(lang dune 3.15)

(name pegboard)

(generate_opam_files true)

(source
 (github matthew-sirman/e-pegboard))

(authors "Matthew Sirman")

(maintainers "Matthew Sirman")

(license LICENSE)

(package
 (name pegboard)
 (synopsis "Badminton e-pegboard app")
 (description "Badminton e-pegboard app")
 (depends
  (ocaml (= 5.2.1))
   dune
  (dune-configurator (= 3.19.1))
   bonsai
   bonsai_web
   core
   js_of_ocaml
   bonsai_web_components
   ppx_html))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
