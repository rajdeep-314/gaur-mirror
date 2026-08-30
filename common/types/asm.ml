(*
    isa.ml
*)


(** PASM instructions with labels of type ['l]. *)
type 'l inst =
    | Li    of  Reg.t * int             (** Load immediate. *)
    | Ld    of  Reg.t * Reg.t * int     (** Load. *)
    | St    of  Reg.t * Reg.t * int     (** Store. *)
    | Add   of  Reg.t * Reg.t * Reg.t   (** Add two Reg.tisters. *)
    | Addi  of  Reg.t * Reg.t * int     (** Add Reg.tister and immediate. *)
    | Sub   of  Reg.t * Reg.t * Reg.t   (** Subtract two Reg.tisters. *)
    | And   of  Reg.t * Reg.t * Reg.t   (** Bitwise and between two Reg.tisters. *)
    | Not   of  Reg.t * Reg.t           (** Bitwise negation of a Reg.tister. *)
    | Sl    of  Reg.t * Reg.t * int     (** Logical Shift Left. *)
    | Sr    of  Reg.t * Reg.t * int     (** Logical Shift Right. *)
    | Beq   of  Reg.t * Reg.t * 'l      (** Branch if equal-to. *)
    | Blt   of  Reg.t * Reg.t * 'l      (** Branch if less-than. *)
    | Jalr  of  Reg.t * Reg.t * int     (** Jump and link Reg.tister. *)
    | Apc   of  Reg.t * int             (** Add to PC and store in a Reg.tister. *)
    | J     of  'l                      (** (Unconditional) jump. *)
    | Label of  'l                      (** Define a label. *)
    | Spin                              (** Spin endlessly. *)

(** PASM programs. *)
type 'l prog = 'l inst list

(** Types with arbitrary annotations. *)

(** Instructions with annotations and labels of types ['a] and ['l],
    respectively. *)
type ('a, 'l) annot_inst = 'l inst * 'a
(** Programs with annotations and labels of types ['a] and ['l],
    respectively. *)
type ('a, 'l) annot_prog = ('a, 'l) annot_inst list

(** Types with strings for labels and syntactic positions as annotations.
    'syn' stands for 'syntactic' here. *)
type syn_inst = (Lexing.position * Lexing.position, string) annot_inst
type syn_prog = (Lexing.position * Lexing.position, string) annot_prog

(** Remove annotations from instructions and programs. *)
let remove_annot_from_inst (i:('a, 'l) annot_inst) : 'l inst = fst i
let remove_annot_from_prog (p:('a, 'l) annot_prog) : 'l prog = List.map fst p


(** Helper functions to write ASM instructions and programs. *)
module Helper = struct
    let li rd n = Li (rd,n)
    let ld rd rs1 n = Ld (rd, rs1, n)
    let st rd rs1 n = St (rd, rs1, n)
    let add rd rs1 rs2 = Add (rd, rs1, rs2)
    let addi rd rs1 n = Addi (rd, rs1, n)
    let sub rd rs1 rs2 = Sub (rd, rs1, rs2)
    let and' rd rs1 rs2 = And (rd, rs1, rs2)
    let not rd rs1 = Not (rd, rs1)
    let sl rd rs n = Sl (rd,rs,n)
    let sr rd rs n  = Sr (rd,rs,n)
    let beq rs1 rs2 l = Beq (rs1, rs2, l)
    let blt rs1 rs2 l = Blt (rs1, rs2, l)
    let jalr rd rs1 n = Jalr (rd, rs1, n)
    let apc rd n = Apc (rd, n)
    let j l = J l
    let label l = Label l
    let spin = Spin
end

