(*
    utils.v
*)

(**
    {1 Utility Module}

    Some utility functions and constants.
*)


From Stdlib Require Import Ascii String.
Open Scope string_scope.


(** {2 Utility constants.} *)

(** [new_line] = '\n' *)
Definition new_line : string := String (ascii_of_nat 10) EmptyString.

(** [tab] = '\t' *)
Definition tab : string := String (ascii_of_nat 9) EmptyString.


(** {2 Utility functions.} *)

(** [digit_to_char n] returns ascii of the digit n. *)
Definition digit_to_char (n:nat) : ascii :=
    ascii_of_nat (n + 48).

(** Auxiliary function to convert a natural number to a string. *)
Fixpoint nat_to_string_aux (time:nat) (n:nat) (acc:string) : string :=
    match time with
    | 0       => "0"
    | S time' =>
            if Nat.ltb n 10 then String (digit_to_char n) acc
            else
                let d   := Nat.modulo n 10 in
                let n'  := Nat.div n 10 in
                nat_to_string_aux time' n' (String (digit_to_char d) acc)
    end.



(* [nat_to_string n] returns the string representation of n. *)
Definition nat_to_string (n:nat) : string :=
    nat_to_string_aux n n "".

