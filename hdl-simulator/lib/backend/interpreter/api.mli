(** High-level, I/O-free API for compiling and simulating GHDL programs. *)

open Common

type compiled

(** [compile sources] parses, toposorts, typechecks, analyses, and reduces
    [sources] into a compiled program ready for simulation. *)
val compile : Source.t list -> (compiled, string) result

(** [compile_string ?name content] compiles a single in-memory source string. *)
val compile_string : ?name:string -> string -> (compiled, string) result

(** [chip_names compiled] returns the names of all simulatable chips in
    [compiled] (including builtin ["Nand"]). *)
val chip_names : compiled -> string list

(** [nand_count compiled chip_name] returns the static number of [Nand]
    gates instantiated by [chip_name] (or [0] if unknown). *)
val nand_count : compiled -> string -> int

type build_phase = Repl.build_phase =
  | Building of { built : int; total : int }
  | Stabilizing of { total : int }

type session

(** [create ?on_progress ?max_nands ~chip_name compiled] instantiates
    [chip_name] from [compiled]. If [max_nands] is given and [nand_count]
    exceeds it, returns [Error] immediately without building the DAG. *)
val create
  :  ?on_progress:(build_phase -> unit)
  -> ?max_nands:int
  -> chip_name:string
  -> compiled
  -> (session, string) result

(** [reset session] resets all internal state and input variables to false,
    stabilizes the circuit, and resets simulation time to 0. *)
val reset : session -> (unit, string) result

(** [time session] returns the current simulation cycle count. *)
val time : session -> int

(** [chip_name session] returns the name of the chip under simulation. *)
val chip_name : session -> string

(** [input_type_string session] returns a human-readable string describing the
    chip's input port type (e.g. [{a: bits 1, b: bits 1}]). *)
val input_type_string : session -> string

(** [input_type session] returns the structured input port type of the chip. *)
val input_type : session -> Cmd.Parse.Types.typ

(** [set_input session v] applies a structured boolean [Value.shape] directly to
    the chip's input variables, returning [Error] if the shape does not match. *)
val set_input : session -> bool Value.shape -> (unit, string) result

(** [output_shape session] stabilizes the circuit and returns the structured
    boolean [Value.shape] of the chip's output. *)
val output_shape : session -> bool Value.shape

type cmd_output =
  | Input_set
  | Output of { time : int; value : string }
  | Clocked of { time : int }
  | Quit

(** [eval_cmd session line] parses, typechecks, and executes a single REPL
    command ([INPUT ...], [OUTPUT], [CLOCK], or [QUIT]). *)
val eval_cmd : session -> string -> (cmd_output, string) result

(** [eval_script session script] executes each non-empty line in [script]
    in order, stopping at the first error or [Quit]. *)
val eval_script : session -> string -> (cmd_output list, string) result
