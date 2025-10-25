(* Proofs related to the assembler. *)

(* Here are some rudimentary ideas. *)
(*Lemma eq_len_ast_isa:*)
(*    forall p : ast.prog,*)
(*        ast.num_insts p - ast.num_labels p = isa.num_insts (translate.ast_prog_to_isa_prog p)*)
(*Theorem eq_len:*)
(*    forall p : ast.prog,*)
(*        ast.num_insts p - ast.num_labels p = machine.num_insts (translate.assemble p)*)

