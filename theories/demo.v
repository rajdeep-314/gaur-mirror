From Stdlib Require Import ZArith List String.
Import ListNotations.
Open Scope string_scope.

From assembler Require reg ast isa assemble.

Import ast.
Import reg.


Definition ast_prog : ast.prog string :=
  [
    label string "x";
    li string r1 10%Z;
    li string r2 20%Z;
    label string "y";
    addi string r1 r0 (-1)%Z;
    add string r2 r1 r1;
    label string "meow";
    j string "y"
  ].

Definition isa_prog : isa.prog := assemble.ast_prog_to_isa string String.eqb ast_prog.

Definition machine_prog : machine.prog := assemble.isa_prog_to_machine isa_prog.

Compute machine.prog_to_string machine_prog.

