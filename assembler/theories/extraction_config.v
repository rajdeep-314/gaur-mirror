(*
    extraction_config.v

    Extraction directives.
*)


From Stdlib Require Extraction.
From assembler Require result bits reg machine assemble.


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
    ].

Extract Inductive ast.t         => "Types.asm_inst"
    [
        "Types.Lli" "Types.Lui" "Types.Ld" "Types.St" "Types.Add"
        "Types.Addi" "Types.Sub" "Types.And" "Types.Not" "Types.Beq"
        "Types.Blt" "Types.Jalr" "Types.Apc" "Types.J" "Types.Label" "Types.Spin"
    ].

Extract Inductive isa.t_        => "Types.isa_inst"
    [
        "Types.I_lli" "Types.I_lui" "Types.I_ld" "Types.I_st" "Types.I_add"
        "Types.I_addi" "Types.I_sub" "Types.I_and" "Types.I_not" "Types.I_beq"
        "Types.I_blt" "Types.I_jalr" "Types.I_apc" "Types.I_j"
    ].

