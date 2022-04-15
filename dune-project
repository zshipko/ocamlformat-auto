(lang dune 3.0)

(name ocamlformat-auto)

(generate_opam_files true)

(source
 (github zshipko/ocamlformat-auto))

(authors "Zach Shipko")

(maintainers "Zach Shipko")

(license MIT)

(documentation https://github.com/zshipko/ocamlformat-auto)

(package
 (name ocamlformat-auto)
 (synopsis "A program to automatically select the correct ocamlformat version")
 (depends ocaml dune cmdliner bos progress)
 (tags
  (ocamlformat)))
