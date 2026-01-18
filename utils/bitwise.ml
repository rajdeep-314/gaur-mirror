(*
    bitwise.ml

    Utility functions related to bitwise operations.
*)


let sign_extend data n_bits =
    if (data lsr (n_bits - 1)) land 0x1 == 1 then
        data lor (lnot (1 lsl n_bits - 1))
    else data

let get_bits value high low =
    let mask = (1 lsl (high - low + 1)) - 1 in
    (value lsr low) land mask


