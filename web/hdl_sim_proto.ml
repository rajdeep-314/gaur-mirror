(** Message protocol between hdl_sim_web (UI thread) and hdl_sim_worker (Web Worker). *)

open Js_of_ocaml

(** Serializable representation of input port types for building dynamic UI controls *)
type wire_typ =
  | WTBits of int
  | WTRecord of (string * wire_typ) list

(** Serializable structured boolean values for pin and bus signals *)
type wire_shape =
  | WSBitvec of bool array
  | WSRecord of (string * wire_shape) list

(** Requests sent from UI to Worker *)
type req =
  | Req_compile of string
      (** Compile GHDL source code *)
  | Req_load of string
      (** Instantiate a specific chip by name *)
  | Req_set_input of wire_shape
      (** Set input pins to given structured boolean values *)
  | Req_output
      (** Read current stabilized output shape *)
  | Req_clock
      (** Advance simulation clock tick *)
  | Req_reset
      (** Reset simulation time and variables in-place to false *)

(** Responses sent from Worker to UI *)
type resp =
  | Resp_compiled of { names : string list; selected : string option }
      (** Compilation succeeded with available chip names *)
  | Resp_progress of { msg : string; pct : int option }
      (** Chip construction or stabilization progress update *)
  | Resp_loaded of
      { chip : string
      ; nands : int
      ; time : int
      ; input_type_str : string
      ; input_type : wire_typ
      }
      (** Chip successfully instantiated with metadata and schema *)
  | Resp_input_set
      (** Input values successfully applied *)
  | Resp_output of { time : int; shape : wire_shape }
      (** Current simulation cycle and output shape *)
  | Resp_clocked of { time : int }
      (** Clock cycle advanced *)
  | Resp_reset of { time : int }
      (** Circuit reset complete *)
  | Resp_error of { msg : string; clear_session : bool }
      (** Error report with flag indicating if active session was invalidated *)

(** Encodes an OCaml value to a JSON string for Worker postMessage *)
let encode (x : 'a) : Js.js_string Js.t =
  Json.output x

(** Decodes a JSON string received from Worker into an OCaml value *)
let decode (s : Js.js_string Js.t) : 'a =
  Json.unsafe_input s

