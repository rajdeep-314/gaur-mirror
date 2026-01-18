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
 (synopsis "A toolchain made for teaching purposes.")
 (description
  "This project contains the toolchain meant to be used for teaching third semester CSE students at IIT Palakkad. The toolchain currently includes a CPU simulator, an HDL simulator, an assembler and a RAM initialization tool (for the CPU simulator)." )
 (depends
   (ocaml (>= 4.14))
   (rocq (>= 9.0))
   dune ocamllex num menhir ocamlgraph)
 (tags
  ("toolchain" "rocq" "coq" "formal verification" "assembler"
   "simulator" "cpu simulator" "hdl" "hdl simulator")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
