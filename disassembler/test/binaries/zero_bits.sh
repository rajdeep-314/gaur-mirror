#!/usr/bin/env bash

set -e

OUT_DIR="binaries"
mkdir -p "$OUT_DIR"

ocaml <<'OCAML'
let out_dir = "binaries"

(* Each entry is:
   name, opcode, invalid_bits

   The opcode occupies bits [31:16].

   Instructions with reserved zero bits:

     add : bit 0
     sub : bit 0
     and : bit 0
     not : bits [5:0]
     sl  : bit 5
     sr  : bit 5
*)

let tests = [
  ("add", 0x0008, 1 lsl 0);
  ("sub", 0x0020, 1 lsl 0);
  ("and", 0x0040, 1 lsl 0);
  ("not", 0x0080, 1 lsl 0);
  ("sl",  0x0100, 1 lsl 5);
  ("sr",  0x0200, 1 lsl 5);
]

let () =
  List.iter (fun (name, opcode, invalid_bits) ->

    (* Put the opcode in bits [31:16]. *)
    let instruction = (opcode lsl 16) lor invalid_bits in

    let filename =
      Printf.sprintf "%s/zero_bits_%s.pmac" out_dir name
    in

    (* PMAC stores instructions in big-endian byte order. *)
    let b0 = (instruction lsr 24) land 0xFF in
    let b1 = (instruction lsr 16) land 0xFF in
    let b2 = (instruction lsr 8) land 0xFF in
    let b3 = instruction land 0xFF in

    let oc = open_out_bin filename in
    output_byte oc b0;
    output_byte oc b1;
    output_byte oc b2;
    output_byte oc b3;
    close_out oc;

    Printf.printf "Generated: %s\n" filename

  ) tests
;;
OCAML