(* Pretty printers. *)


(* ASCII mode:
    Each instruction is printed as a 16-character string of ASCII
    0s and 1s. *)

let bool_to_ascii = function
    | false -> "0"
    | true  -> "1"

let machine_inst_to_ascii inst = String.concat "" (List.map bool_to_ascii inst)

let machine_prog_to_ascii prog = String.concat "\n" (List.map machine_inst_to_ascii prog)


(* Binary mode:
    Each instruction is printed as two ASCII characters. *)

(* Converts a list of booleans representing a natural number in binary,
   in reverse, to an integer.
   Example:
        [ false ; true ; false ; true ]     ->  10  *)
let rec bool_list_to_int = function
    | []        -> 0
    | b :: bs   ->
            let rest = bool_list_to_int bs in
            if b then 2 * rest + 1
            else 2 * rest

let machine_inst_to_bin = function
    | [b0; b1; b2; b3; b4; b5; b6; b7; b8; b9; b10; b11; b12; b13; b14; b15] ->
            let l1 = [b7; b6; b5; b4; b3; b2; b1; b0] in
            let l2 = [b15; b14; b13; b12; b11; b10; b9; b8] in
            let char1_int = bool_list_to_int l1 in
            let char2_int = bool_list_to_int l2 in
            let char1_string = String.make 1 (char_of_int char1_int) in
            let char2_string = String.make 1 (char_of_int char2_int) in
            char1_string ^ char2_string
    | _ ->
            failwith "Impossible. Machine instructions must be 16-length boolean lists."

let machine_prog_to_bin prog = String.concat "" (List.map machine_inst_to_bin prog)

