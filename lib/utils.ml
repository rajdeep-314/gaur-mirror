(*
    utils.ml
*)


let get_bits value high low =
    let mask = (1 lsl (high - low + 1)) - 1 in
    (value lsr low) land mask

let sign_extend value bits =
    let sign_bit = 1 lsl (bits - 1) in
    if value land sign_bit <> 0 then
        value - (1 lsl bits)
    else
        value

let rec flatten_results = function
    | []          -> Ok []
    | Ok v :: rs  ->
      begin
      match flatten_results rs with
      | Error e -> Error e
      | Ok vs   -> Ok (v :: vs)
      end
    | Error e :: _  -> Error e

let read_program file_path =
    let ic = open_in_bin file_path in
    let rec loop acc =
        try
            let hi = input_byte ic in
            let lo = input_byte ic in
            let instr = (hi lsl 8) lor lo in
            loop (instr :: acc)
        with End_of_file ->
            close_in ic ;
            List.rev acc
    in loop []

