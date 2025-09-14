type half = int

type t =
    (* { r0 : half; r1 : half; r2 : half; r3 : half;  *)
    (*   r4 : half; r5 : half; r6 : half; r7 : half;  *)
    (*   r8 : half; r9 : half; r10 : half; r11 : half;  *)
    (*   r12 : half; r13 : half; r14 : half; r15 : half } *)
    { mutable r0 : half; mutable r1 : half; mutable r2 : half; mutable r3 : half; 
      mutable r4 : half; mutable r5 : half; mutable r6 : half; mutable r7 : half; 
      mutable r8 : half; mutable r9 : half; mutable r10 : half; mutable r11 : half; 
      mutable r12 : half; mutable r13 : half; mutable r14 : half; mutable r15 : half }


let create () =
    { r0 = 0; r1 = 0; r2 = 0; r3 = 0; 
      r4 = 0; r5 = 0; r6 = 0; r7 = 0; 
      r8 = 0; r9 = 0; r10 = 0; r11 = 0; 
      r12 = 0; r13 = 0; r14 = 0; r15 = 0 }


let read rf i =
    match i with
    | 0 -> rf.r0
    | 1 -> rf.r1
    | 2 -> rf.r2
    | 3 -> rf.r3
    | 4 -> rf.r4
    | 5 -> rf.r5
    | 6 -> rf.r6
    | 7 -> rf.r7
    | 8 -> rf.r8
    | 9 -> rf.r9
    | 10 -> rf.r10
    | 11 -> rf.r11
    | 12 -> rf.r12
    | 13 -> rf.r13
    | 14 -> rf.r14
    | 15 -> rf.r15
    | _ -> failwith "wrong index"


let write rf i data =
    match i with
    | 0 -> rf.r0 <- data
    | 1 -> rf.r1 <- data
    | 2 -> rf.r2 <- data
    | 3 -> rf.r3 <- data
    | 4 -> rf.r4 <- data
    | 5 -> rf.r5 <- data
    | 6 -> rf.r6 <- data
    | 7 -> rf.r7 <- data
    | 8 -> rf.r8 <- data
    | 9 -> rf.r9 <- data
    | 10 -> rf.r10 <- data
    | 11 -> rf.r11 <- data
    | 12 -> rf.r12 <- data
    | 13 -> rf.r13 <- data
    | 14 -> rf.r14 <- data
    | 15 -> rf.r15 <- data
    | _ -> failwith "wrong index"

