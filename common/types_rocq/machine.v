(*
    machine.v
*)


From Stdlib Require Import ZArith String List.
From rocq_utils Require strings.

Import ListNotations.

Require bv.


(** Number of bits in a machine instruction. *)
Definition inst_size:nat := 32.

(** [t] represents a single machine instruction. *)
Definition t:Type := bv.t inst_size.

(** [prog] represents a machine program, with instructions of type [t]. *)
Definition prog:Type := list t.


(** Various representations of machine instructions and programs. *)
Module Printers.

    (** ASCII representation. *)
    Module Ascii.

        (** [bit true] is ['1'] and [bit false] is ['0']. *)
        Definition bit (b:bool) : Ascii.ascii :=
            Ascii.ascii_of_nat (if b then 49 else 48).

        (** [inst ins] is the string representation of instructrion [ins].
            Each instruction is represented by an [inst_size]-length string
            consisting of '0' and '1' as characters. *)
        Definition inst (ins:t) : string :=
            Vector.fold_right (fun b s => String (bit b) s) ins EmptyString.

        (** [prog p] is the string representation of program [p].
            A program is represented as a bunch of lines, each line being the
            string representation of an instruction in the program. *)
        Definition prog (p:prog) : string :=
            String.concat strings.newline (List.map inst p).

    End Ascii.


    (** Binary representation. *)
    Module Binary.

        (** [byte z shift] extracts the 8-bit byte at the given bit offset
            [shift], counting from the least significant bit. *)
        Definition byte (z:Z) (shift:nat) : Ascii.ascii := Ascii.ascii_of_nat
            (Z.to_nat (Z.modulo (Z.div z (Z.pow 2 (Z.of_nat shift))) 256)).

        (** [inst ins] represents an instruction as four bytes in big-endian.
            NOTE: This assumes [inst_size] being [32]. This is NOT a general
            function! *)
        Definition inst (ins:t) : string :=
            let z := bv.to_int_unsigned ins in
            List.fold_right
                (fun shift s => String (byte z shift) s)
                EmptyString [24; 16; 8; 0].

        (** [prog p] is the binary program representation of a program [p].
            This is simply the four bytes of every instruction in the program
            stitched together. *)
        Definition prog (p:prog) : string :=
            String.concat "" (List.map inst p).

    End Binary.

End Printers.

