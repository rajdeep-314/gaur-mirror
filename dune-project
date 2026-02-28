(lang dune 3.21)
(using menhir 3.0)
(using rocq 0.11)


(name ptools)

(generate_opam_files true)

(source
 (gitlab rajdeep-314/ptools))

(authors "Rajdeep <rajdeep31415@gmail.com>" "Raagam Parmar <raagamparmar@gmail.com>" "M Sreenath Karthick <msreenathkarthick2005@gmail.com>")

(maintainers "Rajdeep <rajdeep31415@gmail.com>" "Raagam Parmar <raagamparmar@gmail.com>" "M Sreenath Karthick <msreenathkarthick2005@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ptools)
 (synopsis "A toolchain made for teaching purposes")
 (depends
   (ocaml (>= 4.14))
   (rocq-core (>= 9.0.0))
   dune num menhir ocamlgraph)
 (tags
  ("toolchain" "rocq" "coq" "formal verification" "assembler"
   "simulator" "cpu simulator" "hdl" "hdl simulator")))

(package
  (name assembler)
  (synopsis "Assembler for PASM")
  (depends
   (ocaml (>= 4.14))
   (rocq-core (>= 9.0.0))
   dune menhir))

(package
  (name disassembler)
  (synopsis "Disassembler for PISA binaries")
 (depends
   (ocaml (>= 4.14))
   dune menhir))

(package
  (name cpu-simulator)
  (synopsis "CPU simulator for a PISA processor")
  (depends
   (ocaml (>= 4.14))
   dune menhir))

(package
  (name hdl-simulator)
  (synopsis "HDL simulator for PHDL")
  (depends
   (ocaml (>= 4.14))
   dune menhir ocamlgraph))

(package
  (name ram-init)
  (synopsis "RAM image initializer for the PISA CPU simulator")
  (depends
   (ocaml (>= 4.14))
   dune num menhir))

(package
  (name vm)
  (allow_empty)
  (synopsis "Stack-based virtual machine")
  (depends
    (ocaml (>= 4.14))
    (rocq-core (>= 9.0.0))
    dune menhir))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
