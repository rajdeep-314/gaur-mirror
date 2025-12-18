(lang dune 3.20)

(name raminit)

(generate_opam_files true)

(source
 (gitlab rajdeep-314/ram-init-pisa))

(authors "Rajdeep <rajdeep31415@gmail.com>")

(maintainers "Rajdeep <rajdeep31415@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name raminit)
 (synopsis "RAM image initializer")
 (description "A tool to convert a textual description of an initial RAM state to a binary image. Made to be used in the CPU simulator for PISA.")
 (depends ocaml dune ocamllex num))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
