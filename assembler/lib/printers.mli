(*
    printers.mli
*)


open Types


val reg_to_string : reg -> string


(* ISA mode (intermediate) printers. *)

val isa_inst_to_string : isa_inst -> string
val isa_prog_to_string : isa_prog -> string

val print_isa_inst : isa_inst -> unit
val print_isa_prog : isa_prog -> unit
val print_isa_prog_with_pc : isa_prog -> int -> unit


(* Machine mode printers. *)

(* ASCII mode. *)
val machine_inst_to_ascii : machine_inst -> string
val machine_prog_to_ascii : machine_prog -> string

(* Binary mode. *)
val machine_inst_to_bin : machine_inst -> string
val machine_prog_to_bin : machine_prog -> string

