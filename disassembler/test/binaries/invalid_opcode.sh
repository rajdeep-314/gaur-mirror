#!/usr/bin/env bash

set -e

OUT_DIR="binaries"
mkdir -p "$OUT_DIR"

ocaml <<'OCAML'
let out_dir = "binaries"

let invalid_opcodes = [
  0x0000;
  0x0003;
  0x0005;
  0x000F;
  0x00FF;
  0x0300;
  0x0FFF;
  0x3000;
  0x7FFF;
  0x8000;
  0xFFFF;
]

let () =
  List.iter (fun opcode ->
    let instruction = opcode lsl 16 in

    let b0 = (instruction lsr 24) land 0xFF in
    let b1 = (instruction lsr 16) land 0xFF in
    let b2 = (instruction lsr 8) land 0xFF in
    let b3 = instruction land 0xFF in

    let filename =
      Printf.sprintf "%s/invalid_opcode_%04x.pmac" out_dir opcode
    in

    let oc = open_out_bin filename in
    output_byte oc b0;
    output_byte oc b1;
    output_byte oc b2;
    output_byte oc b3;
    close_out oc;

    Printf.printf "Generated: %s\n" filename
  ) invalid_opcodes
;;
OCAML