let add_map bs (Ast.Map (addr, data)) =
    let data_high = char_of_int (data lsr 8) in
    let data_low = char_of_int (data land 0xff) in
    begin
    Bytes.set bs (2*addr) data_high;
    Bytes.set bs (2*addr + 1) data_low
    end

let translate_maps ms =
    let bs = Bytes.create 32768 in
    let () = List.iter (add_map bs) ms in
    bs


