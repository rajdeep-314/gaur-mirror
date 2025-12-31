(lang dune 3.18)
(using menhir 2.0)
(using coq 0.10)

(name assembler)

(generate_opam_files true)

(source
 (gitlab rajdeep-314/assembler-pisa))

(authors "Rajdeep <rajdeep31415@gmail.com>" "M Sreenath Karthick <msreenathkarthick2005@gmail.com>")

(maintainers "Rajdeep <rajdeep31415@gmail.com>" "M Sreenath Karthick <msreenathkarthick2005@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name assembler)
 (synopsis "Assembler for PASM")
 (description
  "A Rocq-based assembler for PASM - a RISC-V style assembly based on PISA.
   Generates binary, intermediate assembly and ASCII translations.")
 (depends
   (menhir (>= 2.0))
   (ocaml (>= 4.14))
   (rocq (>= 9.0))
   dune ocamllex)
 (tags
  ("assembler" "rocq" "coq" "formal verification")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
