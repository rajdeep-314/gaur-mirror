(*
    ram.ml

    Type for the RAM and functions on it.
*)


open Syntax

type addr = int             (* address *)
type half = int             (* half word *)

type t = half Array.t


let max_addr = 16384                   (* 2^14 *)
let create () = Array.make (max_addr + 1) 0


let valid_address ram addr =
    addr >= 0 && addr < Array.length ram

let read = Array.get
let write ram addr data =
    let masked_data = data land 0xffff in
    ram.(addr) <- masked_data

let read_safe ram addr =
    if valid_address ram addr then Some ram.(addr)
    else None

let write_safe ram addr data =
    if not (valid_address ram addr) then None
    else
        let masked_data = data land 0xffff in
        Some (ram.(addr) <- data)

let init_from_mmap map =
    let ram = create () in
    let () = List.iter (fun (addr, data) -> write ram addr data) map in
    ram


(* Printer functions. *)

(* Prints the data stored at address addr. *)
let print_at_addr ram addr =
    match read_safe ram addr with
    | None -> print_string "0000"
    | Some v -> Printf.printf "%04x" v

(* Print address-data for all addresses with non-zero data. *)
let print_all_nz ram =
    let print_func addr data =
        if data == 0 then ()
        else Printf.printf "%04x:\t%04x\n" addr data in
    Array.iteri print_func ram


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
    let low, high = min low high, max low high in
    print_in_range_helper ram low high

