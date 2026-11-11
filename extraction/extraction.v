(*
    extraction.v
*)

(** {1 Extraction}

This script is responsible for appropriately extracting modules, types,
definitions, etc. from Rocq theories to appropriate OCaml code.
*)



(** {2 Extraction setup} *)

From Stdlib Require Extraction.
Extraction Language OCaml.

(** Some very useful extraction directives. *)
From Stdlib Require
    ExtrOcamlBasic
    ExtrOcamlNatInt
    ExtrOcamlZInt
    ExtrOcamlNativeString.

(** Blacklisting to avoid name collisions. *)
Extraction Blacklist List String.

Set Extraction Output Directory ".".



(** {2 Stuff to extract from} *)

(** From Rocq theory [rocq_utils]. *)
From rocq_utils Require
    arith
    result
    strings.

(** From Rocq theory [rocq_types]. *)
From rocq_types Require
    reg
    bv
    machine
    isa
    asm.

(** From Rocq theory [assembler]. *)
From assembler Require
    symtable
    errors
    assemble.


(** {2 Extraction directives} *)

(** Length-indexed vectors become ordinary lists, and the nat argument for
    [Vector.cons] is ignored. *)
Extract Inductive Vector.t =>
    "list"
    [ "( [] )" "( fun (x, _, xs) -> x :: xs )" ]
    "(fun fnil fcons xs ->
        match xs with
        | []      -> fnil ()
        | x :: xs -> fcons x 0 xs)".

(** [Datatypes.result] gets mapped to OCaml's [result] type. *)
Extract Inductive Datatypes.result => "result" [ "Ok" "Error" ].


(** {2 Extraction} *)

Separate Extraction
    (* Rocq theory: [rocq_utils]. *)
    arith
    result
    strings

    (* Rocq theory: [rocq_types]. *)
    reg
    bv
    machine
    isa
    asm

    (* Rocq theory: [assembler]. *)
    errors
    symtable
    assemble.

