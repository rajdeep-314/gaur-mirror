(*
    word.ml
*)

(** Contains type for a CPU word and operations on the same. *)

(** A word (type [t]) is a 32-bit integer. *)
type t = int32


(** Functions on [t]. *)

(** [to_string w] is the string representation of word [w]. *)
let to_string : t -> string = Int32.to_string

(** [of_string s] is the word corresponding to string [s]. *)
let of_string : string -> t = Int32.of_string

(** [of_int n] is the word corresponding to int [n].
    WARNING: Probably machine-dependent. *)
let of_int : int -> t = Int32.of_int

(** [to_int w] returns the integer equivalent of word [w].
    WARNING: Probably machine-dependent. *)
let to_int : t -> int = Int32.to_int

(** [to_char w] is the character equivalent of word [w] -- [Some c] as the
    result denotes that the word [w] was in the ASCII characters range ([[32,
    126]]), and [None] denotes otherwise. *)
let to_char : t -> char option =
    fun w ->
    if w >= 32l && w <= 126l then Some (char_of_int (Int32.to_int w))
    else None


(** Binary operations on words (type being [t -> t -> t]). *)

let add  = Int32.add
let sub  = Int32.sub
let mul  = Int32.mul
let div  = Int32.div
let band = Int32.logand                     (** Bitwise and. *)
let bor  = Int32.logor                      (** Bitwise or.  *)

(** Unary operations on words (type being [t -> t]. *)

let neg  = Int32.neg                        (** Integer negation. *)
let bnot = Int32.lognot                     (** Bitwise not. *)

(** Relational operations on words (type being [t -> t -> bool]). *)

let eq : t -> t -> bool = ( == )
let lt : t -> t -> bool = ( < )             (** Signed comparison. *)
let le : t -> t -> bool = ( <= )            (** Signed comparison. *)
let gt : t -> t -> bool = ( > )             (** Signed comparison. *)
let ge : t -> t -> bool = ( >= )            (** Signed comparison. *)
let ne : t -> t -> bool = ( <> )

