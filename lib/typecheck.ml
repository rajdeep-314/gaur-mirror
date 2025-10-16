open Ast

module Env = Map.Make(String)

let (let*) = Result.bind

type tenv = typ Env.t (* Typing envorinment *)
type cenv = (typ *typ) Env.t (* Chip environment *)

type typeError = 
  | EmptyBitVector
  | UnboundChip
  | InstanceArgTypeMismatch
  | RecordSplicing
  | SpliceIndexOutOfRange
  | RecordConcat
  | UnboundIdentifier
  | BitVectorMemberAccess
  | UnknownField
  | RepeatingFieldName


let rec infer (tenv: tenv) (cenv: cenv) e = 
  match e with
  | Constant b ->
    let l = List.length b in
    if l >= 1
    then Ok (TBits l)
    else Error EmptyBitVector

  | Instance (f, e) ->
    let* te = infer tenv cenv e in
    begin match Env.find_opt f cenv with
      | None -> Error UnboundChip
      | Some (t1, t2) -> 
        if t1 = te
        then Ok t2
        else Error InstanceArgTypeMismatch
    end

  | Splice (e, (i, j)) -> 
    let* te = infer tenv cenv e in
    begin match te with
      | TRecord _ -> Error RecordSplicing
      | TBits n ->
        if (0 <= i) && (i <= j) && (j < n)
        then Ok (TBits (j - i + 1))
        else Error SpliceIndexOutOfRange
    end

  | Concat (e1, e2) -> 
    let* te1 = infer tenv cenv e1 in
    let* te2 = infer tenv cenv e2 in
    begin match (te1, te2) with
      | TBits n, TBits m -> Ok (TBits (n + m))
      | TRecord _, TBits _ 
      | TBits _, TRecord _
      | TRecord _, TRecord _ -> Error RecordConcat
    end

  | Record r -> 
    let fields = List.map fst r in
    let values = List.map snd r in

    (* check uniqueness *)
    let has_dupes l =
      let rec aux seen l =
        match l with
        | [] -> false
        | x::xs -> if List.mem x seen then true else aux (x::seen) xs
      in aux [] l
    in
    if has_dupes fields 
    then Error RepeatingFieldName
    else
      let inferred = List.map (infer tenv cenv) values in

      if List.for_all Result.is_ok inferred
      then 
        let safe_inferred = List.map Result.get_ok inferred in
        let tr = List.map2 (fun x y -> (x, y)) fields safe_inferred in
        Ok (TRecord tr)
      else List.find Result.is_error inferred

  | Access i -> 
    begin match Env.find_opt i tenv with
      | Some t -> Ok t
      | None -> Error UnboundIdentifier
    end

  | Member (e, f) -> 
    let* te = infer tenv cenv e in
    begin match te with
      | TBits _ -> Error BitVectorMemberAccess
      | TRecord r -> 
        begin match List.assoc_opt f r with
          | Some t -> Ok t
          | None -> Error UnknownField
        end
    end
