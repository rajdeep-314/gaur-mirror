(*
    utils.v

    Some utility functions and constants.
*)


From Stdlib Require Import Ascii String.
Open Scope string_scope.


(* Utility constants. *)

Definition new_line : string := String (ascii_of_nat 10) EmptyString.
Definition tab : string := String (ascii_of_nat 9) EmptyString.


(* Utility functions. *)

(* Character corresponding to a digit.
   Expected to be used for 0 <= n <= 9. *)
Definition digit_to_char (n:nat) : ascii :=
    ascii_of_nat (n + 48).

(* Auxiliary function to convert a natural number to a string. *)
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


(* Convert a natural number to a string. *)
Definition nat_to_string (n:nat) : string :=
    nat_to_string_aux n n "".

