let sign_extend data n_bits =
    if (data lsr (n_bits - 1)) land 0x1 == 1 then
        data lor (lnot (1 lsl n_bits - 1))
    else data

