(lang dune 3.22)
(using menhir 3.0)
(using rocq 0.11)

(name gaur)

(generate_opam_files true)

(source
 (gitlab verify-at-iitpkd/gaur))

(authors "Rajdeep <rajdeep31415@gmail.com>" "Raagam Parmar <raagamparmar@gmail.com>" "M Sreenath Karthick <msreenathkarthick2005@gmail.com>")

(maintainers "Rajdeep <rajdeep31415@gmail.com>" "Raagam Parmar <raagamparmar@gmail.com>" "M Sreenath Karthick <msreenathkarthick2005@gmail.com>")

(license LICENSE)

(documentation https://verify-at-iitpkd.gitlab.io/gaur)

(package
 (name gaur)
 (synopsis "An assembly to VM toolchain for educational purposes")
 (depends
   (ocaml (>= 4.14))
   (rocq-core (>= 9.0.0))
   dune num menhir ocamlgraph)
 (tags
  ("toolchain" "rocq" "coq" "formal verification" "assembler"
   "simulator" "cpu simulator" "hdl" "hdl simulator" "vm" "stack-based vm" "vm
   simulator" "vm emulator" "vm translator")))

(package
 (allow_empty)
 (name common))

(package
  (name assembler)
  (synopsis "Assembler for Gaur ASM")
  (depends
   (ocaml (>= 4.14))
   (rocq-core (>= 9.0.0))
   dune menhir))

(package
  (name disassembler)
  (synopsis "Disassembler for Gaur binaries")
  (depends
   (ocaml (>= 4.14))
   dune menhir))

(package
  (name cpu-simulator)
  (synopsis "CPU simulator for a Gaur processor")
  (depends
   (ocaml (>= 4.14))
   dune menhir))

(package
  (name hdl-simulator)
  (synopsis "HDL simulator for Gaur HDL")
  (depends
   (ocaml (>= 4.14))
   dune menhir ocamlgraph))

(package
  (name ram-init)
  (synopsis "RAM image initializer for the Gaur CPU simulator")
  (depends
   (ocaml (>= 4.14))
   dune num menhir))

(package
  (name vm)
  (allow_empty)
  (synopsis "Stack-based virtual machine for the Gaur toolchain")
  (depends
    (ocaml (>= 4.14))
    (rocq-core (>= 9.0.0))
    dune menhir))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
