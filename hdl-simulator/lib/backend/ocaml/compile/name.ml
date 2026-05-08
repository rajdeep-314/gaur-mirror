(** {1 Compiling PHDL identifiers to OCaml identifiers}  *)

(** {2 Prefixes} *)

(** Prefix of module name for a chip *)
let name_pfx = "chip_"

(** Prefix for variables in a chip *)
let var_pfx  = "var_"


(** {2 Chip IO} *)

(** [id_input] is ["input"], name of a chip's input type. *)
let id_input = "input"

(** [id_output] is ["output"], name of a chip's output type. *)
let id_output = "output"


(** {2 Chip Variables} *)

(** [var_current id] is the OCaml identifier for the
    current state variable [id].
*)
let var_current id =
  String.lowercase_ascii (var_pfx ^ id)

(** [var_next id] is the OCaml identifier for the
    next state variable [id].
*)
let var_next id =
  String.lowercase_ascii (var_pfx ^ id ^ "'")


(** {2 Chip Interface} *)

(** [module_name id] is the OCaml module identifier of a chip with
    name [id].
*)
let module_name id =
  String.capitalize_ascii (name_pfx ^ id)

(** [chipfld_name id] is the OCaml field name of a chip with
    name [id], to be used in stateful chips.
*)
let chipfld_name id =
  String.lowercase_ascii (id ^ "'")

(** [fun_out] is the name of a chip's, output function. *)
let fun_out = "out"

(** [fun_clk] is the name of a chip's clock function. *)
let fun_clk = "clk"

(** [fun_out_clk] is the name of a chip's cobined
    output and clock function.
*)
let fun_out_clk = "out_clk"

(** [fun_pp_tin] is ["pp_tin"], the name of a chip's input
    expression pretty printer.
*)
let fun_pp_tin = "pp_tin"

(** [fun_pp_tout] is ["pp_tout"], the name of a chip's output
    expression pretty printer.
*)
let fun_pp_tout = "pp_tout"

(** [fun_default_input] is ["default_input"], the name of a chip's
    default input.
*)
let fun_default_input = "default_input"

(** [fun_default_state] is ["default_state"], the name of a chip's
    default state.
*)
let fun_default_state = "default_state"

(** [fun_typeof_tin] is ["typeof_tin"], the name of the value
    containing runtime type information for a chip's input.
*)
let fun_typeof_tin = "typeof_tin"

(** [fun_typeof_tout] is ["typeof_tout"], the name of the value
    containing runtime type information for a chip's output.
*)
let fun_typeof_tout = "typeof_tout"

(** [fun_totype_tin] is ["totype_tin"], the name of the function
    which converts parsed expression to chip's expression type.
*)
let fun_totype_tin = "totype_tin"

(** [fun_totype_tout] is ["totype_tout"], the name of the function
    which converts parsed expression to chip's expression type.
*)
let fun_totype_tout = "totype_tout"
