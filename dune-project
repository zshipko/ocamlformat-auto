(lang dune 3.0)

(name ocamlformat-manager)

(generate_opam_files true)

(source
 (github zshipko/ocamlformat-manager))

(authors "Zach Shipko")

(maintainers "Zach Shipko")

(license MIT)

(documentation https://github.com/zshipko/ocamlformat-manager)

(package
 (name ocamlformat-manager)
 (synopsis "A program to manage ocamlformat installations")
 (depends ocaml dune cmdliner)
 (tags
  (ocamlformat)))
