(** Safe Incremental Hash Tables *)

module type KEY = sig
  type t
end


module type VAL = sig
  type t
end


module Make (K : KEY) (V : VAL) : sig
  type node
  val add  : K.t  -> V.t -> node
  val find : node -> V.t
end = struct
  type t = (K.t, V.t) Hashtbl.t

  type node = K.t

  let tbl : t = Hashtbl.create 16

  let add (k : K.t) (v : V.t) : node =
    Hashtbl.add tbl k v;
    k

  let find (n : node) : V.t =
    Hashtbl.find tbl n
end
