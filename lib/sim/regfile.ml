type half = int
type reg = Ast.Reg.t

type t =
    { mutable r0 : half; mutable r1 : half; mutable r2 : half; mutable r3 : half; 
      mutable r4 : half; mutable r5 : half; mutable r6 : half; mutable r7 : half; 
      mutable r8 : half; mutable r9 : half; mutable r10 : half; mutable r11 : half; 
      mutable r12 : half; mutable r13 : half; mutable r14 : half; mutable r15 : half }


let create () =
    { r0 = 0; r1 = 0; r2 = 0; r3 = 0; 
      r4 = 0; r5 = 0; r6 = 0; r7 = 0; 
      r8 = 0; r9 = 0; r10 = 0; r11 = 0; 
      r12 = 0; r13 = 0; r14 = 0; r15 = 0 }


let read rf = function
    | Ast.Reg.R0 -> rf.r0
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
    | Ast.Reg.R0 -> rf.r0 <- masked_data
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
    match reg with
    | Ast.Reg.R0 -> Printf.printf "r0:\t\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R1 -> Printf.printf "r1:\t\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R2 -> Printf.printf "r2:\t\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R3 -> Printf.printf "r3:\t\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R4 -> Printf.printf "r4:\t\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R5 -> Printf.printf "r5:\t\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R6 -> Printf.printf "r6:\t\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R7 -> Printf.printf "r7:\t\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R8 -> Printf.printf "r8:\t\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R9 -> Printf.printf "r9:\t\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R10 -> Printf.printf "r10:\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R11 -> Printf.printf "r11:\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R12 -> Printf.printf "r12:\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R13 -> Printf.printf "r13:\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R14 -> Printf.printf "r14:\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)
    | Ast.Reg.R15 -> Printf.printf "r15:\t%04x\t\t%d\n" data (Ast.Bit.sign_extend data 16)


let print rf =
    let () = print_reg rf Ast.Reg.R0 in
    let () = print_reg rf Ast.Reg.R1 in
    let () = print_reg rf Ast.Reg.R2 in
    let () = print_reg rf Ast.Reg.R3 in
    let () = print_reg rf Ast.Reg.R4 in
    let () = print_reg rf Ast.Reg.R5 in
    let () = print_reg rf Ast.Reg.R6 in
    let () = print_reg rf Ast.Reg.R7 in
    let () = print_reg rf Ast.Reg.R8 in
    let () = print_reg rf Ast.Reg.R9 in
    let () = print_reg rf Ast.Reg.R10 in
    let () = print_reg rf Ast.Reg.R11 in
    let () = print_reg rf Ast.Reg.R12 in
    let () = print_reg rf Ast.Reg.R13 in
    let () = print_reg rf Ast.Reg.R14 in
    print_reg rf Ast.Reg.R15


