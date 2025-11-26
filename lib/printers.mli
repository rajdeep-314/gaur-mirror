open Types

(* ASCII mode. *)
val machine_inst_to_ascii : machine_inst -> string
val machine_prog_to_ascii : machine_prog -> string

(* Binary mode. *)
val machine_inst_to_bin : machine_inst -> string
val machine_prog_to_bin : machine_prog -> string

