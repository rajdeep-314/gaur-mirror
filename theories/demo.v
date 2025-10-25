From Stdlib Require Import ZArith List String.
Import ListNotations.
Open Scope string_scope.

From assembler Require result reg ast isa assemble.
Import result.

Import ast.
Import reg.


Definition ast_prog : ast.prog string :=
  [
    label string "x";
    lli string r1 10%Z;
    lli string r2 20%Z;
    label string "y";
    addi string r1 r0 (0)%Z;
    add string r2 r1 r1;
    label string "meow";
    j string "y"
  ].


Compute assemble.symtable_of_prog string ast_prog.

Definition machine_prog_r : result machine.prog string := assemble.assemble string String.eqb ast_prog.

Compute
    match machine_prog_r with
    | Error e => e
    | Ok mp => machine.prog_to_string mp
    end.

