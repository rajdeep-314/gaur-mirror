(*
    utils.ml

    Contains some utility functions used by files in this library.
*)


(** [pow2 k] is [2^k] for [k >= 0], and [1] otherwise. *)
let rec pow2 k = if k <= 0 then 1 else 2 * pow2 (k - 1)

(** [cir_u] stands for "Check In Range (Unsigned)" -- [cir_u k i] checks
    whether [k] is in the interval [[0, 2^k - 1]]. If the same holds, [None] is
    returned, otherwise [Some (l, u, n)] is returned, with [l] and [u] being [0]
    and [2^k - 1], respectively, and [n] being [i]. *)
let cir_u k i =
    let lb = 0 and ub = pow2 k - 1 in
    if lb <= i && i <= ub then None else Some (lb, ub, i)

(** [cir_s] is the signed variant of [cir_u] -- [cir_s k i] checks whether [k]
    is in the interval [[-2^(k - 1), 2^(k - 1) - 1]]. If the same holds, [None]
    is returned, otherwise [Some (l, u, n)] is returned, with [l] and [u] being
    [-2^(k - 1)] and [2^(k - 1) - 1], respectively, and [n] being [i]. *)
let cir_s k i =
    let x = pow2 (k - 1) in
    let lb = ~-x and ub = x - 1 in
    if lb <= i && i <= ub then None else Some (lb, ub, i)

