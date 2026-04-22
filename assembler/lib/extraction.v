(*
    extraction.v

    Extraction rules to generate the OCaml file assembler_extr.ml containing
    1. ast_prog_to_isa_prog_extr    - assemble to intermediate assembly
    2. assemble_prog_extr           - assemble to machine program
*)


From assembler_new Require extraction_config.

Set Extraction Output Directory ".".
Extraction "assembler_extr.ml" assemble.ast_prog_to_isa_prog_extr assemble.assemble_prog_extr.

