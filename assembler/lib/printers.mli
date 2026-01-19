(*
    printers.mli
*)


open Types

(** {1 Printer functions}*)

(** {2 Registers}*)

(**
[reg_to_string reg] converts a register to its string representation.
*)
val reg_to_string : reg -> string


(** {2 PISA programs} *)

(**
[isa_inst_to_string inst] converts an ISA instruction to its string
representation.
*)
val isa_inst_to_string : isa_inst -> string

(**
[isa_prog_to_string prog] converts an ISA program to its string
representation.
*)
val isa_prog_to_string : isa_prog -> string

(**
[print_isa_inst inst] prints an ISA instruction to standard output.
*)
val print_isa_inst : isa_inst -> unit

(**
[print_isa_prog prog] prints an ISA program to standard output.
*)
val print_isa_prog : isa_prog -> unit

(**
[print_isa_prog_with_pc prog pc] prints an ISA program to standard output,
highlighting the instruction at the given program counter [pc].
*)
val print_isa_prog_with_pc : isa_prog -> int -> unit


(** {2 Machine programs} *)

(** 
[machine_inst_to_ascii inst] converts a machine instruction to its
string representation in ASCII format. 
*)
val machine_inst_to_ascii : machine_inst -> string

(** 
[machine_prog_to_ascii prog] converts a machine program to its
string representation in ASCII format. 
*)
val machine_prog_to_ascii : machine_prog -> string

(**
[machine_inst_to_bin inst] converts a machine instruction to its
string representation in binary format.
*)
val machine_inst_to_bin : machine_inst -> string

(**
[machine_prog_to_bin prog] converts a machine program to its
string representation in binary format. 
*)
val machine_prog_to_bin : machine_prog -> string

