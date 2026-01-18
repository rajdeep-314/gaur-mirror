(*
    utils.ml

    Some utility functions.
*)


let read_file path =
    let ic = open_in path in
    really_input_string ic (in_channel_length ic)

