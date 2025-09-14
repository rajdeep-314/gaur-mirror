type addr = int             (* address *)
type half = int             (* half word *)

type t = (addr, half) Hashtbl.t


let max_addr = 16384                   (* 2^14 *)
let create () = Hashtbl.create 100

let read ram addr =
    match Hashtbl.find_opt ram addr with
    | None -> 0
    | Some data -> data

let write ram addr data =
    Hashtbl.add ram addr data

let read_safe ram addr =
    if addr < 0 || addr >= max_addr then None
    else Some (read ram addr)

let write_safe ram addr data =
    if addr < 0 || addr >= max_addr then None
    else Some (write ram addr data)

