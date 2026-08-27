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

Extract Inductive reg.t         => "Types.Reg.t"
    [
        "Types.Reg.R0" "Types.Reg.R1" "Types.Reg.R2" "Types.Reg.R3"
        "Types.Reg.R4" "Types.Reg.R5" "Types.Reg.R6" "Types.Reg.R7"
        "Types.Reg.R8" "Types.Reg.R9" "Types.Reg.R10" "Types.Reg.R11"
        "Types.Reg.R12" "Types.Reg.R13" "Types.Reg.R14" "Types.Reg.R15"
        "Types.Reg.R16" "Types.Reg.R17" "Types.Reg.R18" "Types.Reg.R19"
        "Types.Reg.R20" "Types.Reg.R21" "Types.Reg.R22" "Types.Reg.R23"
        "Types.Reg.R24" "Types.Reg.R25" "Types.Reg.R26" "Types.Reg.R27"
        "Types.Reg.R28" "Types.Reg.R29" "Types.Reg.R30" "Types.Reg.R31"
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

