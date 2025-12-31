(lang dune 3.18)
(using menhir 2.0)

(name cpusim)

(generate_opam_files true)

(source
 (gitlab rajdeep-314/cpu-simulator-pisa))

(authors "Rajdeep <rajdeep31415@gmail.com>" "M Sreenath Karthick <msreenathkarthick2005@gmail.com>")

(maintainers "Rajdeep <rajdeep31415@gmail.com>" "M Sreenath Karthick <msreenathkarthick2005@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name cpusim)
 (synopsis "CPU simulator for PISA")
 (description "A simple CPU simulator for PISA - a RISC-V style 16-bit ISA for teaching purposes.")
 (depends ocaml dune ocamllex menhir)
 (tags
  ("cpu simulator" "simulator" "isa")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
