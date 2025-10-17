open Big_int


let big_int_0xff = big_int_of_int 0xff


let rec chars_of_big_int_helper n bn acc =
    if n <= 0 then List.rev acc else
    let (q, r) = quomod_big_int bn big_int_0xff in
    let ch = char_of_int (int_of_big_int r) in
    chars_of_big_int_helper (n - 1) q (ch :: acc)

let chars_of_big_int n bn = chars_of_big_int_helper n bn []

