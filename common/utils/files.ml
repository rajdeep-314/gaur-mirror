(*
    files.ml

    Utility functions related to files.
*)


let read_file path =
    let ic = open_in path in
    really_input_string ic (in_channel_length ic)

let write_file out_path out_content =
    let oc = open_out out_path in
    output_string oc (out_content ^ "\n") ;
    close_out oc

let half_words_of_path file_path =
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

let words_of_path file_path =
    let ic = open_in_bin file_path in
    let rec loop acc =
        try
            let b1 = input_byte ic in
            let b2 = input_byte ic in
            let b3 = input_byte ic in
            let b4 = input_byte ic in
            let instr =
                (b1 lsl 24) lor
                (b2 lsl 16) lor
                (b3 lsl 8) lor
                b4 in
            loop (instr :: acc)
        with End_of_file ->
            close_in ic ;
            List.rev acc
    in loop []

