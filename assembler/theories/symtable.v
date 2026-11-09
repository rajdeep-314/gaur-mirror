(*
    symtable.v
*)

(**
    {1 Symbol Table Module}

    Type for a symbol table, along with functions to manipulate and query the
    table. A symbol table here refers to a table with keys being strings and
    values being bindings of some [A:Type].
*)


From Stdlib Require Import String FMapAVL OrderedTypeEx List.
Import ListNotations.


(** [StringMap] is a finite map with keys of type [string]. *)
Module StringMap := FMapAVL.Make String_as_OT.


(** [bindings A] is essentially a non-empty list of values of [A]. *)
Inductive bindings (A:Type) : Type :=
    | Bindings : A -> list A -> bindings A.

(** Making [A:Type] an implicit argument for constructors [Bindings]. *)
Arguments Bindings {A}.


(** [single_binding x] is a binding with just [x] in it. *)
Definition single_binding {A:Type} (x:A) : bindings A := Bindings x [].

(** [add_binding b x] adds [x] to bindings [b]. The first argument of
    [Bindings] remains the latest binding that was added. *)
Definition add_binding {A:Type} (b:bindings A) (x:A) : bindings A :=
    match b with
    | Bindings y ys => Bindings x (y :: ys)
    end.


(** [t A] represents a symbol table with keys of type [string] and values of
    type [bindings A]. *)
Definition t (A:Type) : Type := StringMap.t (bindings A).


(** [empty] is a symbol table containing no entries. *)
Definition empty (A:Type) : t A := StringMap.empty _.

(** [add m s x] adds the binding [s -> x] to the symbol table [m]. *)
Definition add {A:Type} (m:t A) (s:string) (x:A) : t A :=
    match StringMap.find s m with
    | None   => StringMap.add s (single_binding x) m
    | Some b => StringMap.add s (add_binding b x) m
    end.

(** [find m s] is [Some b] if [b] are the bindings corresponding to [s] in [m].
    In case there are no bindings corresponding to [s] in [m], [find m s] is
    [None]. *)
Definition find {A:Type} (m:t A) (s:string) : option (bindings A) :=
    StringMap.find s m.

