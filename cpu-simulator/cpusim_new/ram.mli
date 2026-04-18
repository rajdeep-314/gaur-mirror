(*
    ram.mli
*)


type addr = int         (* Address.   *)
type word = int         (* word. *)
type t                  (* RAM.       *)

val max_addr : addr

(* Create an all-zeroes RAM. *)
val create : unit -> t
(* Read word at an address. *)
val read : t -> addr -> word
(* Write word at an address. *)
val write : t -> addr -> word -> unit

(* Safe versions of read and write (detect out-of-bound addresses). *)
val read_safe : t -> addr -> word option
val write_safe : t -> addr -> word -> unit option

(* Create a RAM from a address-value map. *)
val init_from_mmap : (addr * word) list -> t


(* Printer functions. *)

(* Print the word at an address. *)
val print_at_addr : t -> addr -> unit
(* Print all non-zero words. *)
val print_all_nz : t -> unit
(* Print words in an address interval. *)
val print_in_range : t -> addr -> addr -> unit

