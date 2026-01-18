(*
    ram.mli
*)


type addr = int         (* Address.   *)
type half = int         (* Half-word. *)
type t                  (* RAM.       *)

val max_addr : addr

(* Create an all-zeroes RAM. *)
val create : unit -> t
(* Read half-word at an address. *)
val read : t -> addr -> half
(* Write half-word at an address. *)
val write : t -> addr -> half -> unit

(* Safe versions of read and write (detect out-of-bound addresses). *)
val read_safe : t -> addr -> half option
val write_safe : t -> addr -> half -> unit option

(* Create a RAM from a address-value map. *)
val init_from_mmap : (addr * half) list -> t


(* Printer functions. *)

(* Print the half-word at an address. *)
val print_at_addr : t -> addr -> unit
(* Print all non-zero half-words. *)
val print_all_nz : t -> unit
(* Print half-words in an address interval. *)
val print_in_range : t -> addr -> addr -> unit

