open Syntax

type half = int
type reg = Ast.Reg.t

type t =
    { mutable r1 : half; mutable r2 : half; mutable r3 : half; 
      mutable r4 : half; mutable r5 : half; mutable r6 : half; mutable r7 : half; 
      mutable r8 : half; mutable r9 : half; mutable r10 : half; mutable r11 : half; 
      mutable r12 : half; mutable r13 : half; mutable r14 : half; mutable r15 : half }


let create () =
    { r1 = 0; r2 = 0; r3 = 0; 
      r4 = 0; r5 = 0; r6 = 0; r7 = 0; 
      r8 = 0; r9 = 0; r10 = 0; r11 = 0; 
      r12 = 0; r13 = 0; r14 = 0; r15 = 0 }


let read rf = function
    | Ast.Reg.R0 -> 0
    | Ast.Reg.R1 -> rf.r1
    | Ast.Reg.R2 -> rf.r2
    | Ast.Reg.R3 -> rf.r3
    | Ast.Reg.R4 -> rf.r4
    | Ast.Reg.R5 -> rf.r5
    | Ast.Reg.R6 -> rf.r6
    | Ast.Reg.R7 -> rf.r7
    | Ast.Reg.R8 -> rf.r8
    | Ast.Reg.R9 -> rf.r9
    | Ast.Reg.R10 -> rf.r10
    | Ast.Reg.R11 -> rf.r11
    | Ast.Reg.R12 -> rf.r12
    | Ast.Reg.R13 -> rf.r13
    | Ast.Reg.R14 -> rf.r14
    | Ast.Reg.R15 -> rf.r15


let write rf i data =
    let masked_data = data land 0xffff in
    match i with
    | Ast.Reg.R0 -> ()
    | Ast.Reg.R1 -> rf.r1 <- masked_data
    | Ast.Reg.R2 -> rf.r2 <- masked_data
    | Ast.Reg.R3 -> rf.r3 <- masked_data
    | Ast.Reg.R4 -> rf.r4 <- masked_data
    | Ast.Reg.R5 -> rf.r5 <- masked_data
    | Ast.Reg.R6 -> rf.r6 <- masked_data
    | Ast.Reg.R7 -> rf.r7 <- masked_data
    | Ast.Reg.R8 -> rf.r8 <- masked_data
    | Ast.Reg.R9 -> rf.r9 <- masked_data
    | Ast.Reg.R10 -> rf.r10 <- masked_data
    | Ast.Reg.R11 -> rf.r11 <- masked_data
    | Ast.Reg.R12 -> rf.r12 <- masked_data
    | Ast.Reg.R13 -> rf.r13 <- masked_data
    | Ast.Reg.R14 -> rf.r14 <- masked_data
    | Ast.Reg.R15 -> rf.r15 <- masked_data


let print_reg rf reg =
    let data = read rf reg in
    let data_sext = Ast.Bit.sign_extend data 16 in
    let reg_n = Ast.Reg.to_int reg in
    if reg_n > 9 then
        Printf.printf "r%d:\t\t%04x\t\t%d" (Ast.Reg.to_int reg) data data_sext
    else
        Printf.printf "r%d:\t\t\t%04x\t\t%d" (Ast.Reg.to_int reg) data data_sext

let print rf =
    let print_func reg =
        let () = print_reg rf reg in print_endline "" in
    List.iter print_func Ast.Reg.Helper.reg_list

