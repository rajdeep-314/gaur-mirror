(*
    isa.v
*)


From Stdlib Require Import String.
From common.types Require bv reg.

Import bv List.ListNotations.

Open Scope string_scope.
Open Scope list_scope.


(** {1 ISA instructions and programs} *)

(** [inst] denotes an ISA instruction. *)
Variant inst : Type :=
    | Li   : reg.t -> bv.t 11 -> inst
    | Ld   : reg.t -> reg.t -> bv.t 6 -> inst
    | St   : reg.t -> reg.t -> bv.t 6 -> inst
    | Add  : reg.t -> reg.t -> reg.t -> inst
    | Addi : reg.t -> reg.t -> bv.t 6 -> inst
    | Sub  : reg.t -> reg.t -> reg.t -> inst
    | And  : reg.t -> reg.t -> reg.t -> inst
    | Not  : reg.t -> reg.t -> inst
    | Sl   : reg.t -> reg.t -> bv.t 5 -> inst
    | Sr   : reg.t -> reg.t -> bv.t 5 -> inst
    | Beq  : reg.t -> reg.t -> bv.t 6 -> inst
    | Blt  : reg.t -> reg.t -> bv.t 6 -> inst
    | Jalr : reg.t -> reg.t -> bv.t 6 -> inst
    | Apc  : reg.t -> bv.t 11 -> inst
    | J    : bv.t 16 -> inst.

(** [prog] denotes ISA programs. *)
Definition prog : Type := list inst.


(** {1 Annotated instructions and programs} *)

(** [annot_inst A] denotes an ISA instruction (of type [inst]) annotated with
    something of type [A]. *)
Definition annot_inst {A:Type} : Type := inst * A.

(** [inst_of_annot_inst i] returns the annotation-less instruction from the
    annotated instruction [i]. *)
Definition inst_of_annot_inst {A:Type} : @annot_inst A -> inst := fst.

(** [annot_of_annot_inst i] is the annotation associated with annotated
    instruction [i]. *)
Definition annot_of_annot_inst {A:Type} : @annot_inst A -> A := snd.

(** [annot_prog A] denotes an ISA program (of type [prog]), with each
    instruction annotated with something of type [A]. *)
Definition annot_prog {A:Type} : Type := list (@annot_inst A).

(** [prog_of_annot_prog p] returns the program corresponding to the annotated
    program [p]. *)
Definition prog_of_annot_prog {A:Type} : @annot_prog A -> prog :=
    List.map inst_of_annot_inst.



(** [Printers] contains string representations of ISA instructions and
    programs. *)
Module Printers.

    Local Notation "x ^^ y" := (append x (append " " y))
        (at level 60, right associativity).

    (** [inst i] is the string representation of ISA instruction [i]. *)
    Definition inst (i:inst) : string :=
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
        | Beq rd rs i    => "beq" ^^ regs [rd; rs] ^^ bvs i
        | Blt rd rs i    => "beq" ^^ regs [rd; rs] ^^ bvs i
        | Jalr rd rs i   => "jalr" ^^ regs [rd; rs] ^^ bvs i
        | Apc rd i       => "apc" ^^ reg rd ^^ bvs i
        | J i            => "j" ^^ bvs i
        end.

    (** [prog i] is the string representation of ISA program [p]. *)
    Definition prog (p:prog) : string := concat "\n" (List.map inst p).

End Printers.

