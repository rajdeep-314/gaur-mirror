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
Extract Inductive bits.bv_      => "Types.machine_inst"  [ "(fun l -> l)" ].

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

Extract Inductive ast.t         => "Types.asm_inst"
    [
        "Types.Li" "Types.Ld" "Types.St" "Types.Add"
        "Types.Addi" "Types.Sub" "Types.And" "Types.Not" 
        "Types.Sl" "Types.Sr" "Types.Beq"
        "Types.Blt" "Types.Jalr" "Types.Apc" "Types.J" "Types.Label" "Types.Spin"
    ].

Extract Inductive isa.t_        => "Types.isa_inst"
    [
        "Types.I_li" "Types.I_ld" "Types.I_st" "Types.I_add"
        "Types.I_addi" "Types.I_sub" "Types.I_and" "Types.I_not"
        "Types.I_sl" "Types.I_sr" "Types.I_beq"
        "Types.I_blt" "Types.I_jalr" "Types.I_apc" "Types.I_j"
    ].

