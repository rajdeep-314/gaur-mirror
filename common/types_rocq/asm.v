(*
    asm.v
*)


From Stdlib Require Import String.
From rocq_types Require bv reg.
From rocq_utils Require strings.

Import bv List.ListNotations.

Open Scope string_scope.
Open Scope list_scope.


(** {1 ASM instructions and programs} *)

(** [inst L] denotes ASM instructions with labels of type [L]. *)
Variant inst {L:Type} : Type :=
    | Li    : reg.t -> bv.t 11 -> inst
    | Ld    : reg.t -> reg.t -> bv.t 6 -> inst
    | St    : reg.t -> reg.t -> bv.t 6 -> inst
    | Add   : reg.t -> reg.t -> reg.t -> inst
    | Addi  : reg.t -> reg.t -> bv.t 6 -> inst
    | Sub   : reg.t -> reg.t -> reg.t -> inst
    | And   : reg.t -> reg.t -> reg.t -> inst
    | Not   : reg.t -> reg.t -> inst
    | Sl    : reg.t -> reg.t -> bv.t 5 -> inst
    | Sr    : reg.t -> reg.t -> bv.t 5 -> inst
    | Beq   : reg.t -> reg.t -> L -> inst
    | Blt   : reg.t -> reg.t -> L -> inst
    | Jalr  : reg.t -> reg.t -> bv.t 6 -> inst
    | Apc   : reg.t -> bv.t 11 -> inst
    | J     : L -> inst
    | Label : L -> inst
    | Spin  : inst.

(** [prog L] denotes ASM programs with labels of type [L]. *)
Definition prog {L:Type} : Type := list (@inst L).


(** {1 Annotated instructions and programs} *)

(** [annot_inst L A] denotes an instruction of type [inst L] annotated with
    something of type [A]. *)
Definition annot_inst {L A:Type} : Type := @inst L * A.

(** [inst_of_annot_inst i] returns the annotation-less instruction from the
    annotated instruction [i]. *)
Definition inst_of_annot_inst {L A:Type} : @annot_inst L A -> @inst L := fst.

(** [annot_of_annot_inst i] is the annotation associated with annotated
    instruction [i]. *)
Definition annot_of_annot_inst {L A:Type} : @annot_inst L A -> A := snd.

(** [annot_prog L A] denotes a program of type [prog L], with each instruction
    annotated with something of type [A]. *)
Definition annot_prog {L A:Type} : Type := list (@annot_inst L A).

(** [prog_of_annot_prog p] returns the program corresponding to the annotated
    program [p]. *)
Definition prog_of_annot_prog {L A:Type} : @annot_prog L A -> @prog L :=
    List.map inst_of_annot_inst.


(** [Printers] contains string representations of ASM instructions and
    programs. *)
Module Printers.

    Local Notation "x ^^ y" := (append x (append " " y))
        (at level 60, right associativity).

    (** [inst lbl i] is the string representation of assembly instruction
        [i], with the function [lbl:L -> string] being the function that gives
        string representations of labels of type [L]. *)
    Definition inst {L:Type} (lbl:L -> string) (i:@inst L) : string :=
        let reg := reg.to_string in
        let regs rs := concat " " (List.map reg rs) in
        let bvu {n} := @bv.Printers.decimal_unsigned n in
        let bvs {n} := @bv.Printers.decimal_signed n in
        match i with
        | Li r i         => "li" ^^ reg r ^^ bvu i
        | Ld rd rs i     => "ld" ^^ regs [rd; rs] ^^ bvs i
        | St rd rs i     => "st" ^^ regs [rd; rs] ^^ bvs i
        | Add rd rs1 rs2 => "add" ^^ regs [rd; rs1; rs2]
        | Addi rd rs i   => "addi" ^^ regs [rd; rs] ^^ bvs i
        | Sub rd rs1 rs2 => "sub" ^^ regs [rd; rs1; rs2]
        | And rd rs1 rs2 => "and" ^^ regs [rd; rs1; rs2]
        | Not rd rs      => "not" ^^ regs [rd; rs]
        | Sl rd rs i     => "sl" ^^ regs [rd; rs] ^^ bvu i
        | Sr rd rs i     => "sr" ^^ regs [rd; rs] ^^ bvu i
        | Beq rd rs l    => "beq" ^^ regs [rd; rs] ^^ lbl l
        | Blt rd rs l    => "blt" ^^ regs [rd; rs] ^^ lbl l
        | Jalr rd rs i   => "jalr" ^^ regs [rd; rs] ^^ bvs i
        | Apc rd i       => "apc" ^^ reg rd ^^ bvs i
        | J l            => "j" ^^ lbl l
        | Label l        => append (lbl l) ":"
        | Spin           => "spin"
        end.

    (** [prog lbl p] is the string representation of assembly program [p],
        with the function [lbl:L -> string] being the function that gives
        string representations of labels of type [L]. *)
    Definition prog {L:Type} (lbl:L -> string) (p:@prog L) : string :=
        concat strings.newline (List.map (@inst L lbl) p).

End Printers.

