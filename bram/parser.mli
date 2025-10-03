module Ram = Cpusim.Ram


val read_half : Bytes.t -> int -> int
val parse_bytes : Bytes.t -> Ram.t
val parse_file : string -> Ram.t

