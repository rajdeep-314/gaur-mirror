(*
    printers.ml

    Pretty printers.
*)


open Types

(* Register printing. *)

let reg_to_string = function
    | R0    -> "r0"  | R1    -> "r1"  | R2    -> "r2"  | R3    -> "r3"
    | R4    -> "r4"  | R5    -> "r5"  | R6    -> "r6"  | R7    -> "r7"
    | R8    -> "r8"  | R9    -> "r9"  | R10   -> "r10" | R11   -> "r11"
    | R12   -> "r12" | R13   -> "r13" | R14   -> "r14" | R15   -> "r15"
    | R16   -> "r16" | R17   -> "r17" | R18   -> "r18" | R19   -> "r19"
    | R20   -> "r20" | R21   -> "r21" | R22   -> "r22" | R23   -> "r23"
    | R24   -> "r24" | R25   -> "r25" | R26   -> "r26" | R27   -> "r27"
    | R28   -> "r28" | R29   -> "r29" | R30   -> "r30" | R31   -> "r31"



(* ISA format printers. *)

let isa_inst_to_string inst =
    let rts = reg_to_string in
    let its = string_of_int in
    match inst with
    | I_li (rd,imm)         -> String.concat " " ["li"; rts rd;its imm]
    | I_ld (rd, rs, imm)    -> String.concat " " ["ld"; rts rd; rts rs; its imm]
    | I_st (rd, rs, imm)    -> String.concat " " ["st"; rts rd; rts rs; its imm]
    | I_add (rd, rs1, rs2)  -> String.concat " " ["add"; rts rd; rts rs1; rts rs2]
    | I_addi (rd, rs1, imm) -> String.concat " " ["addi"; rts rd; rts rs1; its imm]
    | I_sub (rd, rs1, rs2)  -> String.concat " " ["sub"; rts rd; rts rs1; rts rs2]
    | I_and (rd, rs1, rs2)  -> String.concat " " ["and"; rts rd; rts rs1; rts rs2]
    | I_not (rd, rs)        -> String.concat " " ["not"; rts rd; rts rs]
    | I_sl (rd,rs,imm)      -> String.concat " " ["sl"; rts rd;rts rs;its imm]
    | I_sr (rd,rs,imm)      -> String.concat " " ["sr"; rts rd;rts rs;its imm]
    | I_beq (rs1, rs2, imm) -> String.concat " " ["beq"; rts rs1; rts rs2; its imm]
    | I_blt (rs1, rs2, imm) -> String.concat " " ["blt"; rts rs1; rts rs2; its imm]
    | I_jalr (rd, rs, imm)  -> String.concat " " ["jarl"; rts rd; rts rs; its imm]
    | I_apc (rd, imm)       -> String.concat " " ["apc"; rts rd; its imm]
    | I_j imm               -> String.concat " " ["j"; its imm]

let isa_prog_to_string prog = String.concat "\n" (List.map isa_inst_to_string prog)


(* Some printing functions for ISA instructions and programs. *)

let print_isa_inst inst = print_string (isa_inst_to_string inst)
let print_isa_prog p = print_string (isa_prog_to_string p)

let rec print_isa_prog_with_pc prog pc =
    if pc < 0 then print_isa_prog prog
    else
    match prog with
    | [] ->
        if pc = 0 then print_endline "<- PC"
        else ()
    | inst :: prog' ->
        if pc = 0 then
            let () = print_isa_inst inst in
            let () = print_endline "\t\t<- PC" in
            print_isa_prog prog'
        else
            let () = print_isa_inst inst in
            let () = print_endline "" in
            print_isa_prog_with_pc prog' (pc - 1)



(* Machine format printers. *)

(* ASCII mode:
    Each instruction is printed as a 16-character string of ASCII
    0s and 1s. *)

let bool_to_ascii = function
    | false -> "0"
    | true  -> "1"

let machine_inst_to_ascii inst =
    String.concat "" (List.map bool_to_ascii inst)

let machine_prog_to_ascii prog =
    String.concat "\n" (List.map machine_inst_to_ascii prog)


(* Binary mode:
    Each instruction is printed as two ASCII characters. *)

(* Converts a list of booleans representing a natural number in binary,
   in reverse, to an integer.
   Example:
        [ false ; true ; false ; true ]     ->  10  *)
let rec bool_list_to_int = function
    | []        -> 0
    | b :: bs   ->
        let rest = bool_list_to_int bs in
        if b then 2 * rest + 1
        else 2 * rest

let machine_inst_to_bin = function
    | [b0; b1; b2; b3; b4; b5; b6; b7;
       b8; b9; b10; b11; b12; b13; b14; b15;
       b16; b17; b18; b19; b20; b21; b22; b23;
       b24; b25; b26; b27; b28; b29; b30; b31] ->
        let l1 = [b7; b6; b5; b4; b3; b2; b1; b0] in
        let l2 = [b15; b14; b13; b12; b11; b10; b9; b8] in
        let l3 = [b23; b22; b21; b20; b19; b18; b17; b16] in
        let l4 = [b31; b30; b29; b28; b27; b26; b25; b24] in
        let char1_int = bool_list_to_int l1 in
        let char2_int = bool_list_to_int l2 in
        let char3_int = bool_list_to_int l3 in
        let char4_int = bool_list_to_int l4 in
        String.concat "" (List.map (Fun.compose (String.make 1) char_of_int)
            [char1_int; char2_int; char3_int; char4_int])
    | _ ->
        failwith
        "Impossible. Machine instructions must be 32-length boolean lists."

let machine_prog_to_bin prog =
    String.concat "" (List.map machine_inst_to_bin prog)

