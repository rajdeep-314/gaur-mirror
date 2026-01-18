(*
    ast.mli
*)


module T := Assembler.Types

type reg = T.reg =
    | R0  | R1  | R2  | R3
    | R4  | R5  | R6  | R7
    | R8  | R9  | R10 | R11
    | R12 | R13 | R14 | R15


type t = T.isa_inst =
    | I_lli   of  reg * int
    | I_lui   of  reg * int
    | I_ld    of  reg * reg * int
    | I_st    of  reg * reg * int
    | I_add   of  reg * reg * reg
    | I_addi  of  reg * reg * int
    | I_sub   of  reg * reg * reg
    | I_and   of  reg * reg * reg
    | I_not   of  reg * reg
    | I_beq   of  reg * reg * int
    | I_blt   of  reg * reg * int
    | I_jalr  of  reg * reg * int
    | I_apc   of  reg * int
    | I_j     of  int

type prog = t list

module Helper : sig
    (* Instructions. *)
    val lli : reg -> int -> t
    val lui : reg -> int -> t
    val ld  : reg -> reg -> int -> t
    val st  : reg -> reg -> int -> t
    val add : reg -> reg -> reg -> t
    val addi: reg -> reg -> int -> t
    val sub : reg -> reg -> reg -> t
    val and_: reg -> reg -> reg -> t
    val not : reg -> reg -> t
    val beq : reg -> reg -> int -> t
    val blt : reg -> reg -> int -> t
    val jalr: reg -> reg -> int -> t
    val apc : reg -> int -> t
    val j   : int -> t

    (* Some useful functions. *)
    val reg_to_int : reg -> int
    val reg_list : reg list
    val reg_of_int : int -> reg
end

