(** Reduced AST used by the interpreter.

    It is obtained from [Parse.Ast] by {!Reduce}: every variable
    access is annotated with its kind and every chip call is annotated
    with an integer which is unique within the enclosing chip.
    The integer is used as the key under which the state of that
    particular child instance is stored.
*)

open Hdlutils

module StrMap = Map.Make (String)
module IntMap = Map.Make (Int)

type tipe =
  | TBitvec of int
  | TRecord of tipe StrMap.t

(** Kind of a variable occurring in an expression. *)
type var =
  | Input   (** the input port of the chip *)
  | State   (** a state variable of the chip (its current value) *)
  | Inter   (** an intermediate wire defined by a [:=] assignment *)

type expr =
  | Const  of Bit.t NonEmpty.t
  | Record of expr StrMap.t
  | Splice of expr * int * int
  | Concat of expr * expr
  | Access of var * string
  | Member of expr * string

type rhs =
  | Call of string * int * expr (** [int] carries the instance id *)
  | Expr of expr

type assign =
  | Current of string * rhs
  | Next    of string * rhs

type port =
  { name : string
  ; tipe : tipe
  }

type states = tipe StrMap.t

type chip =
  { name   : string
  ; input  : port
  ; output : port
  ; states : states
  ; parts  : assign list
  }

type prog = chip list
