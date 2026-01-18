(*
    parser.ml

    Parser for the inital RAM image (binary RAM).
*)


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

let parse_bytes bs size =
    let ram = Ram.create () in
    let () = process_bytes bs ram 0 (size / 2 - 1) in
    ram

let parse_file path =
    let bs = Bytes.of_string (Utils.Files.read_file path) in
    let size = Bytes.length bs in
    parse_bytes bs size

