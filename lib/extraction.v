From Stdlib Require Extraction.
From assembler Require result bits reg machine assemble.


Extraction Language OCaml.
From Stdlib Require ExtrOcamlBasic ExtrOcamlNatInt ExtrOcamlZInt ExtrOcamlNativeString.

Extract Inductive result.result => "Result.t"  [ "Result.Ok" "Result.Error" ].
Extract Inductive bits.t        => "bool"  [ "false" "true" ].
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
Extract Constant machine.prog_erased    => "Types.machine_prog".
Extract Constant ast.imm                => "int".


Set Extraction Output Directory ".".
Extraction "assembler_extr.ml" assemble.assemble_prog_extr.

