(*
    ast.ml

    Captures the abstract syntax of PASM.
*)


module Bit = struct
    type t = B0 | B1

    let zero = B0
    let one = B1

    type imm4 = Imm4 of t * t * t * t
    type imm8 = Imm8 of t * t * t * t * t * t * t * t
    type imm12 = Imm12 of t * t * t * t * t * t * t * t * t * t * t * t

    let int_to_bit = function
        | 1 -> B1
        | _ -> B0

    (* Converts an integer to an n-bit binary number (reverse order). *)
    let rec int_to_n_bin num n =
        if n <= 0 then []
        else
            int_to_bit (num land 0x1) :: int_to_n_bin (num lsr 1) (n - 1)

    let rec bin_to_int = function
        | [] -> 0
        | B0 :: bs -> 2 * bin_to_int bs
        | B1 :: bs -> 2 * bin_to_int bs + 1

    let imm4_to_int = function
        | Imm4 (b3, b2, b1, b0) -> bin_to_int [b0; b1; b2; b3]

    let imm8_to_int = function
        | Imm8 (b7, b6, b5, b4, b3, b2, b1, b0) ->
                bin_to_int [b0; b1; b2; b3; b4; b5; b6; b7]

    let imm12_to_int = function
        | Imm12 (b11, b10, b9, b8, b7, b6, b5, b4, b3, b2, b1, b0) ->
                bin_to_int [b0; b1; b2; b3; b4; b5; b6; b7; b8; b9; b10; b11]


    let int_to_imm4 num =
        match List.rev (int_to_n_bin num 4) with
        | [b0; b1; b2; b3] -> Imm4 (b0, b1, b2, b3)
        | _ -> failwith "Impossible (int_to_imm4)"

    let int_to_imm8 num =
        match List.rev (int_to_n_bin num 8) with
        | [b0; b1; b2; b3; b4; b5; b6; b7] -> Imm8 (b0, b1, b2, b3, b4, b5, b6, b7)
        | _ -> failwith "Impossible (int_to_imm8)"

    let int_to_imm12 num =
        match List.rev (int_to_n_bin num 12) with
        | [b0; b1; b2; b3; b4; b5; b6; b7; b8; b9; b10; b11] ->
                Imm12 (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)
        | _ -> failwith "Impossible (int_to_imm12)"

    (* Only the least significant n_bits remain the same, rest become 0. *)
    let bit_mask data n_bits = data land (1 lsl n_bits - 1)

    let sign_extend data n_bits =
        if (data lsr (n_bits - 1)) land 0x1 == 1 then
            data lor (lnot (1 lsl n_bits - 1))
        else data
end


module Reg = struct
    type t =
        | R0 | R1 | R2 | R3
        | R4 | R5 | R6 | R7
        | R8 | R9 | R10 | R11
        | R12 | R13 | R14 | R15

    let to_int = function
        | R0 -> 0 | R1 -> 1 | R2 -> 2 | R3 -> 3
        | R4 -> 4 | R5 -> 5 | R6 -> 6 | R7 -> 7
        | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11
        | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

    let to_string = function
        | R0 -> "r0" | R1 -> "r1" | R2 -> "r2" | R3 -> "r3"
        | R4 -> "r4" | R5 -> "r5" | R6 -> "r6" | R7 -> "r7"
        | R8 -> "r8" | R9 -> "r9" | R10 -> "r10" | R11 -> "r11"
        | R12 -> "r12" | R13 -> "r13" | R14 -> "r14" | R15 -> "r15"

    module Helper = struct
        let r0 = R0
        let r1 = R1
        let r2 = R2
        let r3 = R3
        let r4 = R4
        let r5 = R5
        let r6 = R6
        let r7 = R7
        let r8 = R8
        let r9 = R9
        let r10 = R10
        let r11 = R11
        let r12 = R12
        let r13 = R13
        let r14 = R14
        let r15 = R15

        let reg_list = [r0; r1; r2; r3; r4; r5; r6; r7; r8; r9; r10; r11; r12; r13; r14; r15]
    end

    let of_int n =
        if n < 0 || n > 15 then
            failwith "Error: Invalid register index. Must be in [0, 15]."
        else 
            List.nth Helper.reg_list n
end


type t =
    | Lli of Reg.t * Bit.imm8
    | Lui of Reg.t * Bit.imm8
    | Ld of Reg.t * Reg.t * Bit.imm4
    | St of Reg.t * Reg.t * Bit.imm4
    | Add of Reg.t * Reg.t * Reg.t
    | Addi of Reg.t * Reg.t * Bit.imm4
    | Sub of Reg.t * Reg.t * Reg.t
    | And of Reg.t * Reg.t * Reg.t
    | Not of Reg.t * Reg.t
    | Beq of Reg.t * Reg.t * Bit.imm4
    | Blt of Reg.t * Reg.t * Bit.imm4
    | Jalr of Reg.t * Reg.t * Bit.imm4
    | Apc of Reg.t * Bit.imm8
    | J of Bit.imm12

type prog = t list

module Helper = struct
    let lli rd imm = Lli (rd, Bit.int_to_imm8 imm)
    let lui rd imm = Lui (rd, Bit.int_to_imm8 imm)
    let ld rd rs imm = Ld (rd, rs, Bit.int_to_imm4 imm)
    let st rs1 rs2 imm = St (rs1, rs2, Bit.int_to_imm4 imm)
    let add rd rs1 rs2 = Add (rd, rs1, rs2)
    let addi rd rs imm = Addi (rd, rs, Bit.int_to_imm4 imm)
    let sub rd rs1 rs2 = Sub (rd, rs1, rs2)
    let and_ rd rs1 rs2 = And (rd, rs1, rs2)
    let not rd rs = Not (rd, rs)
    let beq rs1 rs2 imm = Beq (rs1, rs2, Bit.int_to_imm4 imm)
    let blt rs1 rs2 imm = Blt (rs1, rs2, Bit.int_to_imm4 imm)
    let jalr rd rs imm = Jalr (rd, rs, Bit.int_to_imm4 imm)
    let apc rd imm = Apc (rd, Bit.int_to_imm8 imm)
    let j imm = J (Bit.int_to_imm12 imm)
end


let to_string = function
    | Lli (rd, imm) ->
            "lli " ^ Reg.to_string rd ^ " " ^ string_of_int (Bit.imm8_to_int imm)
    | Lui (rd,imm) -> 
            "lui " ^ Reg.to_string rd ^ " " ^ string_of_int (Bit.imm8_to_int imm)
    | Ld (rd,rs,imm) ->
            "ld " ^ Reg.to_string rd ^ " " ^ Reg.to_string rs ^ " " ^
            string_of_int (Bit.sign_extend (Bit.imm4_to_int imm) 4)
    | St (rs1,rs2,imm) ->
            "st " ^ Reg.to_string rs1 ^ " " ^ Reg.to_string rs2 ^ " " ^
            string_of_int (Bit.sign_extend (Bit.imm4_to_int imm) 4)
    | Add (rd,rs1,rs2) ->
            "add " ^ Reg.to_string rd ^ " " ^ Reg.to_string rs1 ^ " " ^ Reg.to_string rs2
    | Addi (rd,rs,imm) ->
            "addi " ^ Reg.to_string rd ^ " " ^ Reg.to_string rs ^ " " ^
            string_of_int (Bit.sign_extend (Bit.imm4_to_int imm) 4)
    | Sub (rd,rs1,rs2) ->
            "sub " ^ Reg.to_string rd ^ " " ^ Reg.to_string rs1 ^ " " ^ Reg.to_string rs2
    | And (rd,rs1,rs2) ->
            "and " ^ Reg.to_string rd ^ " " ^ Reg.to_string rs1 ^ " " ^ Reg.to_string rs2
    | Not (rd,rs) ->
            "not " ^ Reg.to_string rd ^ " " ^ Reg.to_string rs 
    | Beq (rs1,rs2,imm) ->
            "beq " ^ Reg.to_string rs1 ^ " " ^ Reg.to_string rs2 ^ " " ^
            string_of_int (Bit.sign_extend (Bit.imm4_to_int imm) 4)
    | Blt (rs1,rs2,imm) ->
            "blt " ^ Reg.to_string rs1 ^ " " ^ Reg.to_string rs2 ^ " " ^
            string_of_int (Bit.sign_extend (Bit.imm4_to_int imm) 4)
    | Jalr (rd,rs,imm) ->
            "jalr " ^ Reg.to_string rd ^ " " ^ Reg.to_string rs ^ " " ^
            string_of_int (Bit.sign_extend (Bit.imm4_to_int imm) 4)
    | Apc (rd,imm) ->
            "apc " ^ Reg.to_string rd ^ " " ^
            string_of_int (Bit.sign_extend (Bit.imm8_to_int imm) 8)
    | J imm ->
            "j " ^ string_of_int (Bit.sign_extend (Bit.imm12_to_int imm) 12)


let print_inst inst = print_string (to_string inst)

let print_prog =
    let print_func inst =
        let () = print_inst inst in
        print_endline "" in
    List.iter print_func


let rec print_prog_with_pc prog pc =
    if pc < 0 then print_prog prog
    else
        match prog with
        | [] ->
                if pc = 0 then print_endline ">"
                else ()
        | inst :: prog' ->
                if pc = 0 then
                    let () = print_endline ">" in
                    let () = print_inst inst in
                    let () = print_endline "\n>" in
                    print_prog prog'
                else
                    let () = print_inst inst in
                    let () = print_endline "" in
                    print_prog_with_pc prog' (pc - 1)


