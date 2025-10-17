open Utils


let add_map bs (Ast.Map (addr, data)) word_size lendianb offset =
    let base_addr = (offset + addr) * word_size in
    let cs = chars_of_big_int word_size data in
    (* If little-endian. *)
    if lendianb then
        List.iteri
            (fun i c ->
                Bytes.set bs (base_addr + word_size - 1 - i) c) cs
    (* If big-endian. *)
    else
        List.iteri
            (fun i c ->
                Bytes.set bs (base_addr + i) c) cs


let translate_maps ms ram_size word_size lendianb offset padding =
    let nbytes = ram_size * word_size in
    let padding_bytes = chars_of_big_int word_size padding in
    let init_f i = List.nth padding_bytes (i mod word_size) in
    let bs = Bytes.init nbytes init_f in
    let () = List.iter
        (fun m -> add_map bs m word_size lendianb offset) ms in
    bs

