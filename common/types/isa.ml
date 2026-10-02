(*
    isa.ml
*)


(** Gaur ISA instructions, without labels. *)
type inst =
    | Li    of  Reg.t * int
    | Ld    of  Reg.t * Reg.t * int
    | St    of  Reg.t * Reg.t * int
    | Add   of  Reg.t * Reg.t * Reg.t
    | Addi  of  Reg.t * Reg.t * int
    | Sub   of  Reg.t * Reg.t * Reg.t
    | And   of  Reg.t * Reg.t * Reg.t
    | Not   of  Reg.t * Reg.t
    | Sl    of  Reg.t * Reg.t * int
    | Sr    of  Reg.t * Reg.t * int
    | Beq   of  Reg.t * Reg.t * int
    | Blt   of  Reg.t * Reg.t * int
    | Jalr  of  Reg.t * Reg.t * int
    | Apc   of  Reg.t * int
    | J     of  int

(** Gaur ISA programs. *)
type prog = inst list

(** Types with arbitrary annotations. *)

(** Instructions with annotations of type 'a. *)
type 'a annot_inst = inst * 'a
(** Programs with annotations of type 'a. *)
type 'a annot_prog = 'a annot_inst list

(** Types with syntactic positions as annotations.
    'syn' stands for 'syntactic' here. *)
type syn_inst = (Lexing.position * Lexing.position) annot_inst
type syn_prog = (Lexing.position * Lexing.position) annot_prog

(** Remove annotations from instructions and programs. *)
let remove_annot_from_inst (i:'a annot_inst) : inst = fst i
let remove_annot_from_prog (p:'a annot_prog) : prog = List.map fst p


(** String representations of ISA instructions and programs. *)
module Printers = struct
    module R = Reg.Printers

    (** String representation of an ISA instruction. *)
    let inst i =
        let rts = R.reg in
        let its = string_of_int in
        match i with
        | Li (rd,imm)         -> String.concat " " ["li"; rts rd;its imm]
        | Ld (rd, rs, imm)    -> String.concat " " ["ld"; rts rd; rts rs; its imm]
        | St (rd, rs, imm)    -> String.concat " " ["st"; rts rd; rts rs; its imm]
        | Add (rd, rs1, rs2)  -> String.concat " " ["add"; rts rd; rts rs1; rts rs2]
        | Addi (rd, rs1, imm) -> String.concat " " ["addi"; rts rd; rts rs1; its imm]
        | Sub (rd, rs1, rs2)  -> String.concat " " ["sub"; rts rd; rts rs1; rts rs2]
        | And (rd, rs1, rs2)  -> String.concat " " ["and"; rts rd; rts rs1; rts rs2]
        | Not (rd, rs)        -> String.concat " " ["not"; rts rd; rts rs]
        | Sl (rd,rs,imm)      -> String.concat " " ["sl"; rts rd;rts rs;its imm]
        | Sr (rd,rs,imm)      -> String.concat " " ["sr"; rts rd;rts rs;its imm]
        | Beq (rs1, rs2, imm) -> String.concat " " ["beq"; rts rs1; rts rs2; its imm]
        | Blt (rs1, rs2, imm) -> String.concat " " ["blt"; rts rs1; rts rs2; its imm]
        | Jalr (rd, rs, imm)  -> String.concat " " ["jalr"; rts rd; rts rs; its imm]
        | Apc (rd, imm)       -> String.concat " " ["apc"; rts rd; its imm]
        | J imm               -> String.concat " " ["j"; its imm]

    (** String representation of an ISA program. *)
    let prog p = String.concat "\n" (List.map inst p)

    (** Printing ISA instructions and programs to standard output. *)
    let print_inst = Fun.compose print_string inst
    let print_prog = Fun.compose print_string prog

    (** Printing an ISA program with a marker pointing to the instruction
        corresponding to [pc]. *)
    let rec print_prog_with_pc prog pc =
        if pc < 0 then print_prog prog
        else match prog with
        | [] ->
            if pc = 0 then print_endline "<- PC"
            else ()
        | inst :: prog' ->
            if pc = 0 then
                let () = print_inst inst in
                let () = print_endline "\t\t<- PC" in
                print_prog prog'
            else
                let () = print_inst inst in
                let () = print_endline "" in
                print_prog_with_pc prog' (pc - 1)
end


(** Helper functions to write ISA instructions and programs. *)
module Helper = struct
    let li rd n = Li (rd,n)
    let ld rd rs1 n = Ld (rd, rs1, n)
    let st rd rs1 n = St (rd, rs1, n)
    let add rd rs1 rs2 = Add (rd, rs1, rs2)
    let addi rd rs1 n = Addi (rd, rs1, n)
    let sub rd rs1 rs2 = Sub (rd, rs1, rs2)
    let and' rd rs1 rs2 = And (rd, rs1, rs2)
    let not rd rs1 = Not (rd, rs1)
    let sl rd rs n  = Sl (rd,rs,n)
    let sr rd rs n  = Sr (rd,rs,n)
    let beq rs1 rs2 l = Beq (rs1, rs2, l)
    let blt rs1 rs2 l = Blt (rs1, rs2, l)
    let jalr rd rs1 n = Jalr (rd, rs1, n)
    let apc rd n = Apc (rd, n)
    let j l = J l
end

