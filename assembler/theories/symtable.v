(*
    symtable.v
*)

(**
    {1 Symbol Table Module}

    Type for a symbol table,along with functions to manipulate and query the table.
*)


From Stdlib Require Import String List.
Import ListNotations.
Open Scope string_scope.

From assembler Require Import utils.


(** Type for a symbol table mapping A to nat. *)
Definition t (A:Set) : Set := list (A * nat).

(** An empty symbol table. *)
Definition empty (A:Set) : t A := [].

(** Add an entry to a symbol table (oblivious to duplicates). *)
Definition add (A:Set) (tbl:t A) (lbl:A) (n:nat) : t A :=
    (lbl, n) :: tbl.

(** Look for an entry in a symbol table.
   eq is the equality function for A. *)
Fixpoint find_opt (A:Set) (eq:A -> A -> bool) (tbl:t A) (lbl:A): option nat :=
    match tbl with
    | []                => None
    | (l, n) :: tbl'    =>
            if eq l lbl then Some n
            else find_opt A eq tbl' lbl
    end.

(** Check a symbol table for duplicates.
   Gives None if there are none, else gives Some (l, n, n') - indicating
   label l having two definitions, one at line number n and one at n'. *)
Fixpoint dup (A:Set) (eqb:A -> A -> bool) (tbl:t A) : option (A * nat * nat) :=
    match tbl with
    | []                => None
    | (l, n) :: tbl'    =>
            match find_opt A eqb tbl' l with
            | None      => dup A eqb tbl'
            | Some n'   => Some (l, n, n')
            end
    end.

