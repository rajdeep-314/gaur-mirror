(*
    asm.ml
*)


(** Gaur ASM instructions with labels of type ['l]. *)
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

(** Gaur ASM programs. *)
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

(** Extract annotation from an annotated instruction. *)
let annot_of (i:('a, 'l) annot_inst) : 'a = snd i


(** Module [Validate] contains functions to validate ASM instructions and
    programs. *)
module Validate = struct
    open Common_types_utils

    (** ['a verr] denotes a validation error, with ['a] being the type of
        annotations for instructions. *)
    type 'a verr =

        (** [VErr (a, l, u, n)] denotes that an instruction with annotation [a]
            was invalid, with an out-of-bound immediate [n], the valid
            immediate range being the interval [[l, u]]. *)
        | VErr of 'a * int * int * int

    let string_of_verr ap (VErr (a, l, u, n)) = Printf.sprintf
        "%s\nImmediate validation Error: Immediate %d is outside the valid interval [%d, %d]."
        (ap a) n l u

    (** [inst_partial i] returns [None] if annotated instruction [i]'s
        immediate is in the valid interval, and if that is not the case, [Some
        (l, u, n)] is returned, with the valid interval being [[l, u]], and the
        offending immediate being [n]. *)
    let inst_partial (i:('a, 'l) annot_inst) : (int * int * int) option =
        match remove_annot_from_inst i with
        | Li (_, i)            -> cir_u 11 i
        | Ld (_, _, i)         -> cir_s 6 i
        | St (_, _, i)         -> cir_s 6 i
        | Addi (_, _, i)       -> cir_s 6 i
        | Sl (_, _, i)         -> cir_u 5 i
        | Sr (_, _, i)         -> cir_u 5 i
        | Jalr (_, _, i)       -> cir_s 6 i
        | Apc (_, i)           -> cir_s 11 i
        | Add _ | Sub _ | And _
        | Not _ | Beq _ | Blt _
        | J _ | Label _ | Spin -> None

    (** [inst i], for some annotated instruction [i] returns [None] if the
        instruction [i] has its immediate (if any) in the valid range, and if
        that is not the case, [Some (VErr (a, l, u))] is returned, with [a]
        being the annotation corresponding to [i], and the valid immediate
        interval being [[l, u]]. *)
    let inst (i:('a, 'l) annot_inst) : 'a verr option =
        match inst_partial i with
        | None           -> None
        | Some (u, l, n) -> Some (VErr (annot_of i, u, l, n))

    (** [prog p] returns [None] if all of its instructions have valid immediate
        arguments, and if that is not the case, [Some (VErr (a, l, u, n))] is
        returned, with [a] being the first offending instruction's annotation,
        [[l, u]] being the valid immediate interval, and [n] being the
        offending instruction's immediate. *)
    let rec prog (p:('a, 'l) annot_prog) : 'a verr option =
        match p with
        | []      -> None
        | i :: p' ->
            match inst i with
            | None -> prog p'
            | ow   -> ow
end


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

