(*
    machine.ml
*)


(** Machine instructions. *)
type inst = bool list

(** Machine programs. *)
type prog = inst list


(** String representations of machine instructions and programs. *)
module Printers = struct
    (** ASCII mode:
        Each instruction is printed as a 16-character string of ASCII
        0s and 1s. *)

    let bool_to_ascii = function
        | false -> "0"
        | true  -> "1"

    (** ASCII representation of machine instruction [inst]. *)
    let ascii_inst inst = String.concat "" (List.map bool_to_ascii inst)
    (** ASCII representation of machine program [prog]. *)
    let ascii_prog prog = String.concat "\n" (List.map ascii_inst prog)


    (** Binary mode:
        Each instruction is printed as two ASCII characters. *)

    (** Converts a list of booleans representing a natural number in binary,
       in reverse, to an integer.
       Example:
            [ false ; true ; false ; true ]     ->  10  *)
    let rec bool_list_to_int = function
        | []        -> 0
        | b :: bs   ->
            let rest = bool_list_to_int bs in
            if b then 2 * rest + 1
            else 2 * rest

    (** Binary representation of machine instruction. *)
    let bin_inst = function
        | [b0; b1; b2; b3; b4; b5; b6; b7;
           b8; b9; b10; b11; b12; b13; b14; b15;
           b16; b17; b18; b19; b20; b21; b22; b23;
           b24; b25; b26; b27; b28; b29; b30; b31] ->
            let l1 = [b7; b6; b5; b4; b3; b2; b1; b0] in
            let l2 = [b15; b14; b13; b12; b11; b10; b9; b8] in
            let l3 = [b23; b22; b21; b20; b19; b18; b17; b16] in
            let l4 = [b31; b30; b29; b28; b27; b26; b25; b24] in
            let char1_int = bool_list_to_int l1 in
            let char2_int = bool_list_to_int l2 in
            let char3_int = bool_list_to_int l3 in
            let char4_int = bool_list_to_int l4 in
            String.concat "" (List.map (Fun.compose (String.make 1) char_of_int)
                [char1_int; char2_int; char3_int; char4_int])
        (* NOTE: Maybe this failwith thing could be handled better? *)
        | _ ->
            failwith
            "Impossible. Machine instructions must be 32-length boolean lists."
    (** Binary representation of machine program [prog]. *)
    let bin_prog prog =
        String.concat "" (List.map bin_inst prog)
end

