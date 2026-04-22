(*
    regfile.ml

    Type for a register file and some functions on the same.
*)


module Ast = Syntax.Ast
module M = Utils.Bitwise
module H = Ast.Helper

type word = int         (* 32-bit word *)
type reg = Ast.reg      (* Register   *)

(* Register-file. *)
type t =
    { mutable r1 : word; mutable r2 : word; mutable r3 : word; 
      mutable r4 : word; mutable r5 : word; mutable r6 : word; mutable r7 : word; 
      mutable r8 : word; mutable r9 : word; mutable r10 : word; mutable r11 : word; 
      mutable r12 : word; mutable r13 : word; mutable r14 : word; mutable r15 : word;
      mutable r16 : word; mutable r17 : word; mutable r18 : word; mutable r19 : word;
      mutable r20 : word; mutable r21 : word; mutable r22 : word; mutable r23 : word;
      mutable r24 : word; mutable r25 : word; mutable r26 : word; mutable r27 : word;
      mutable r28 : word; mutable r29 : word; mutable r30 : word; mutable r31 : word }

let create () =
    { r1 = 0; r2 = 0; r3 = 0; 
      r4 = 0; r5 = 0; r6 = 0; r7 = 0; 
      r8 = 0; r9 = 0; r10 = 0; r11 = 0; 
      r12 = 0; r13 = 0; r14 = 0; r15 = 0;
      r16 = 0; r17 = 0; r18 = 0; r19 = 0;
      r20 = 0; r21 = 0; r22 = 0; r23 = 0;
      r24 = 0; r25 = 0; r26 = 0; r27 = 0;
      r28 = 0; r29 = 0; r30 = 0; r31 = 0 }

let read rf = function
    | Ast.R0 -> 0
    | Ast.R1 -> rf.r1   | Ast.R2 -> rf.r2   | Ast.R3 -> rf.r3
    | Ast.R4 -> rf.r4   | Ast.R5 -> rf.r5   | Ast.R6 -> rf.r6   | Ast.R7 -> rf.r7
    | Ast.R8 -> rf.r8   | Ast.R9 -> rf.r9   | Ast.R10 -> rf.r10 | Ast.R11 -> rf.r11
    | Ast.R12 -> rf.r12 | Ast.R13 -> rf.r13 | Ast.R14 -> rf.r14 | Ast.R15 -> rf.r15
    | Ast.R16 -> rf.r16 | Ast.R17 -> rf.r17 | Ast.R18 -> rf.r18 | Ast.R19 -> rf.r19
    | Ast.R20 -> rf.r20 | Ast.R21 -> rf.r21 | Ast.R22 -> rf.r22 | Ast.R23 -> rf.r23
    | Ast.R24 -> rf.r24 | Ast.R25 -> rf.r25 | Ast.R26 -> rf.r26 | Ast.R27 -> rf.r27
    | Ast.R28 -> rf.r28 | Ast.R29 -> rf.r29 | Ast.R30 -> rf.r30 | Ast.R31 -> rf.r31

let write rf i data =
    let masked_data = data land 0xffffffff in
    match i with
    | Ast.R0 -> ()
    | Ast.R1 -> rf.r1 <- masked_data   | Ast.R2 -> rf.r2 <- masked_data
    | Ast.R3 -> rf.r3 <- masked_data   | Ast.R4 -> rf.r4 <- masked_data
    | Ast.R5 -> rf.r5 <- masked_data   | Ast.R6 -> rf.r6 <- masked_data
    | Ast.R7 -> rf.r7 <- masked_data   | Ast.R8 -> rf.r8 <- masked_data
    | Ast.R9 -> rf.r9 <- masked_data   | Ast.R10 -> rf.r10 <- masked_data
    | Ast.R11 -> rf.r11 <- masked_data | Ast.R12 -> rf.r12 <- masked_data
    | Ast.R13 -> rf.r13 <- masked_data | Ast.R14 -> rf.r14 <- masked_data
    | Ast.R15 -> rf.r15 <- masked_data
    | Ast.R16 -> rf.r16 <- masked_data | Ast.R17 -> rf.r17 <- masked_data
    | Ast.R18 -> rf.r18 <- masked_data | Ast.R19 -> rf.r19 <- masked_data
    | Ast.R20 -> rf.r20 <- masked_data | Ast.R21 -> rf.r21 <- masked_data
    | Ast.R22 -> rf.r22 <- masked_data | Ast.R23 -> rf.r23 <- masked_data
    | Ast.R24 -> rf.r24 <- masked_data | Ast.R25 -> rf.r25 <- masked_data
    | Ast.R26 -> rf.r26 <- masked_data | Ast.R27 -> rf.r27 <- masked_data
    | Ast.R28 -> rf.r28 <- masked_data | Ast.R29 -> rf.r29 <- masked_data
    | Ast.R30 -> rf.r30 <- masked_data | Ast.R31 -> rf.r31 <- masked_data

let print_reg rf reg =
    let data = read rf reg in
    let data_sext = M.sign_extend data 32 in
    let reg_n = H.reg_to_int reg in
    if reg_n > 9 then
        Printf.printf "r%d:\t\t%08x\t\t%d" reg_n data data_sext
    else
        Printf.printf "r%d:\t\t\t%08x\t\t%d" reg_n data data_sext

let print rf =
    let print_func reg =
        let () = print_reg rf reg in print_endline "" in
    List.iter print_func H.reg_list
