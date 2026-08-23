(*
    extraction_config.v

    Extraction directives.
*)


From Stdlib Require Extraction.
From assembler_new Require result bits reg machine assemble.


Extraction Language OCaml.
From Stdlib Require ExtrOcamlBasic ExtrOcamlNatInt ExtrOcamlZInt ExtrOcamlNativeString.


(* The result type in theories/result.v to OCaml's Result.t *)
Extract Inductive result.result => "Result.t"  [ "Result.Ok" "Result.Error" ].

(* Bits to booleans. *)
Extract Inductive bits.t        => "bool"  [ "false" "true" ].

(* Non-indexed bitvectors to lists. *)
Extract Inductive bits.bv_      => "Types.Machine.inst"  [ "(fun l -> l)" ].

Extract Inductive reg.t         => "Types.reg"
    [
        "Types.R0" "Types.R1" "Types.R2" "Types.R3"
        "Types.R4" "Types.R5" "Types.R6" "Types.R7"
        "Types.R8" "Types.R9" "Types.R10" "Types.R11"
        "Types.R12" "Types.R13" "Types.R14" "Types.R15"
        "Types.R16" "Types.R17" "Types.R18" "Types.R19"
        "Types.R20" "Types.R21" "Types.R22" "Types.R23"
        "Types.R24" "Types.R25" "Types.R26" "Types.R27"
        "Types.R28" "Types.R29" "Types.R30" "Types.R31"
    ].

Extract Inductive ast.t         => "Types.Asm.inst"
    [
        "Types.Asm.Li"
        "Types.Asm.Ld" "Types.Asm.St"
        "Types.Asm.Add" "Types.Asm.Addi" "Types.Asm.Sub"
        "Types.Asm.And" "Types.Asm.Not"
        "Types.Asm.Sl" "Types.Asm.Sr"
        "Types.Asm.Beq" "Types.Asm.Blt"
        "Types.Asm.Jalr" "Types.Asm.Apc" "Types.Asm.J"
        "Types.Asm.Label" "Types.Asm.Spin"
    ].

Extract Inductive isa.t_        => "Types.Isa.inst"
    [
        "Types.Isa.Li"
        "Types.Isa.Ld" "Types.Isa.St"
        "Types.Isa.Add" "Types.Isa.Addi" "Types.Isa.Sub"
        "Types.Isa.And" "Types.Isa.Not"
        "Types.Isa.Sl" "Types.Isa.Sr"
        "Types.Isa.Beq" "Types.Isa.Blt"
        "Types.Isa.Jalr" "Types.Isa.Apc" "Types.Isa.J"
    ].

