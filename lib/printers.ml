(* Pretty printers. *)

let bool_to_ascii = function
    | false -> "0"
    | true  -> "1"

let machine_inst_to_ascii inst = String.concat "" (List.map bool_to_ascii inst)

let machine_prog_to_ascii prog = String.concat "\n" (List.map machine_inst_to_ascii prog)


