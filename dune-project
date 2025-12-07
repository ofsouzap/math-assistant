(lang dune 3.20)

(name math-assistant)

(generate_opam_files true)

(source
 (github ofsouzap/math-assistant))

(authors "Ofsouzap <ofsouzap@gmail.com>")

(maintainers "Ofsouzap <ofsouzap@gmail.com>")

(license MIT)

(using menhir 3.0)

(package
 (name math-assistant)
 (synopsis "Math assistant thingy")
 (depends ocaml core lwt quickterface))
