(*
    alias.v

    Register aliases, etc.
*)


From assembler Require reg.

Module R := assembler.reg.


(* Register aliases. *)
Definition zero : R.t := R.r0.  (* Zero-register. *)
Definition sp : R.t := R.r1.    (* Global stack pointer. *)
Definition lp : R.t := R.r2.    (* Local segment pointer. *)
Definition ap : R.t := R.r3.    (* Argument segment pointer. *)

(* "Working" registers (r8 - r15). *)
Definition w1 : R.t := R.r8.
Definition w2 : R.t := R.r9.
Definition w3 : R.t := R.r10.
Definition w4 : R.t := R.r11.
Definition w5 : R.t := R.r12.
Definition w6 : R.t := R.r13.
Definition w7 : R.t := R.r14.
Definition w8 : R.t := R.r15.

