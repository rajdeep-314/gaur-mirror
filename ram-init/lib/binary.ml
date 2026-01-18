(*
    binary.ml

    Representation for binary data.
*)


type t = Bytes.t

let to_string = Bytes.to_string
let print bs = print_string (to_string bs)

