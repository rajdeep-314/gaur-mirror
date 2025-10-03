module Ram = Cpusim.Ram


let read_half bs n =
    let high = int_of_char (Bytes.get bs (2*n)) in
    let low = int_of_char (Bytes.get bs (2*n + 1)) in
    (high lsl 8) lor low


let rec process_bytes bs ram n max_n =
    if n > max_n then ()
    else
        let data = read_half bs n in
        let () = Ram.write ram n data in
        process_bytes bs ram (n + 1) max_n


let parse_bytes bs =
    let ram = Ram.create () in
    let () = process_bytes bs ram 0 16383 in
    ram

let parse_file path =
    parse_bytes (Bytes.of_string (Utils.Files.read_file path))

