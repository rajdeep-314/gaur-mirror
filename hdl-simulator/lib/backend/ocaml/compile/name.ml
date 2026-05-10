(** {1 Compiling PHDL identifiers to OCaml identifiers}  *)

(** {2 Prefixes} *)

(** Prefix of module name for a chip *)
let name_pfx = ""

(** Prefix for variables in a chip *)
let var_pfx  = "var_"


(** {2 Chip Interface} *)

(** [id_input] is ["input"], name of a chip's input type. *)
let id_input = "input"

(** [id_output] is ["output"], name of a chip's output type. *)
let id_output = "output"

(** [id_state] is ["state"], name of a chip's state type. *)
let id_state = "state"


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


(** {2 Chip Module Interface} *)

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


(** {2 Primary functions} *)

(** [fun_output] is, ["output"], the name of a chip's output function. *)
let fun_output = "output"

(** [fun_clock] is ["clock"], the name of a chip's clock function. *)
let fun_clock = "clock"

(** [fun_out_clk] is ["out_clk"], the name of a chip's cobined
    output and clock function.
*)
let fun_out_clk = "out_clk"


(** {2 Pretty-printing functions} *)

(** [fun_pp_tin] is ["pp_tin"], the name of a chip's input
    expression pretty printer.
*)
let fun_pp_tin = "pp_tin"

(** [fun_pp_tout] is ["pp_tout"], the name of a chip's output
    expression pretty printer.
*)
let fun_pp_tout = "pp_tout"


(** {2 Default Values} *)

(** [fun_default_input] is ["default_input"], the name of a chip's
    default input.
*)
let fun_default_input = "default_input"

(** [fun_default_state] is ["default_state"], the name of a chip's
    default state.
*)
let fun_default_state = "default_state"


(** {2 Runtime typing information} *)

(** [fun_typ_of_input] is ["typ_of_input"], the name of the value
    containing runtime type information for a chip's input.
*)
let fun_typ_of_input = "typ_of_input"

(** [fun_typ_of_output] is ["typ_of_output"], the name of the value
    containing runtime type information for a chip's output.
*)
let fun_typ_of_output = "typ_of_output"


(** {2 Conversion from-and-to runtime expressions} *)

(** {3 From runtime expressions} *)

(** [fun_input_of_expr] is ["input_of_expr"], the name of the function
    which converts parsed expression to chip's expression type.
*)
let fun_input_of_expr = "input_of_expr"

(** [fun_output_of_expr] is ["output_of_expr"], the name of the function
    which converts parsed expression to chip's expression type.
*)
let fun_output_of_expr = "output_of_expr"

(** {3 To runtime expressions} *)

(** [fun_expr_of_input] is ["expr_of_input"], the name of the function
    which converts chip's input to a runtime expression.
*)
let fun_expr_of_input = "expr_of_input"

(** [fun_expr_of_output] is ["expr_of_output"], the name of the function
    which converts chip's output to a runtime expression.
*)
let fun_expr_of_output = "expr_of_output"


(** {2 Chip select for REPL} *)

(** [fun_select] is ["select"], the name of the function which selects
    chip given its name.
*)
let fun_select = "select"
