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
    if 0 > addr || addr >= max_addr then
        print_endline "RAM warning: Attempted write to out of bounds address."
    else
    let masked_data = data land 0xffff in
    if masked_data == 0 then Hashtbl.remove ram addr
    else Hashtbl.replace ram addr masked_data

let read_safe ram addr =
    if addr < 0 || addr >= max_addr then None
    else Some (read ram addr)

let write_safe ram addr data =
    if addr < 0 || addr >= max_addr then None
    else Some (write ram addr data)


(* Printer functions. *)

(* Prints the data stored at address addr. *)
let print_at_addr ram addr =
    match Hashtbl.find_opt ram addr with
    | None -> print_string "0000"
    | Some v -> Printf.printf "%04x" v

(* Print address-data for all addresses with non-zero data. *)
let print_all_nz ram =
    let addrs = Hashtbl.fold (fun k _ acc -> k :: acc) ram [] in
    let sorted_addrs = List.sort Int.compare addrs in
    let print_func addr = Printf.printf "%04x:\t%04x\n" addr (Hashtbl.find ram addr) in
    List.iter print_func sorted_addrs


let rec print_in_range_helper ram addr max_addr =
    if addr > max_addr then ()
    else
        let () = Printf.printf "%04x:\t" addr in
        let () = print_at_addr ram addr in
        let () = print_endline "" in
        print_in_range_helper ram (addr + 1) max_addr

let print_in_range ram low high =
    let low = max 0 low in
    let high = min max_addr high in
    print_in_range_helper ram low high

