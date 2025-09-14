(* Add comments. *)

module Bit :
    sig
        type t
        val zero : t
        val one : t
        type imm4
        type imm8
        type imm12
    end


module Reg :
    sig
        type t
        val to_int : t -> int
    end

type t

