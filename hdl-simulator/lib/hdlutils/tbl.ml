(** Safe Incremental Hash Tables *)

module type KEY = sig
  type t
end


module type VAL = sig
  type t
end


module Make (K : KEY) (V : VAL) : sig
  type node
  val add      : K.t  -> V.t -> node
  val add_list : (K.t * V.t) list -> (K.t * node) list
  val find     : node -> V.t
end = struct
  type t = (K.t, V.t) Hashtbl.t

  type node = K.t

  let tbl : t = Hashtbl.create 16

  let add (k : K.t) (v : V.t) : node =
    match Hashtbl.find_opt tbl k with
    | Some _ -> k
    | None   ->
      Hashtbl.add tbl k v;
      k

  let add_list (kv : (K.t * V.t) list) : (K.t * node) list =
    List.map
      (fun (k, v) -> (k, add k v))
      kv

  let find (n : node) : V.t =
    Hashtbl.find tbl n
end
