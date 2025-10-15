open Ast

module Env = Map.Make(String)

type tenv = typ Env.t (* Typing envorinment *)
type cenv = (typ *typ) Env.t (* Chip environment *)

let rec infer e (tenv: tenv) (cenv: cenv) : typed_expr = 
  match e with
  | Constant b ->
    let l = List.length b in
    if l >= 1
    then { e=e; t=TBits l }
    else failwith "Non positive bits"

  | Instance (f, e) ->
    let te = infer e tenv cenv in
    begin match Env.find_opt f cenv with
      | None -> failwith ("Unbound chip " ^ f)
      | Some (t1, t2) ->
        if t1 = te.t
        then {e=e; t=t2 }
        else failwith "type mismatch"
    end

  | Splice (e, (i, j)) -> 
    let te = infer e tenv cenv in
    begin match te.t with
      | TRecord _ -> failwith "can not splice a record"
      | TBits n ->
        if n <= 0
        then failwith "Bitvctor has to be non empty"
        else 
          if (0 <= i) && (i <= j) && (j < n)
          then { e=e; t=TBits (j - i + 1) }
          else failwith "Bitvector splicing out of range"
    end

  | Concat (e1, e2) -> 
    let te1 = infer e1 tenv cenv in
    let te2 = infer e2 tenv cenv in
    begin match (te1.t, te2.t) with
      | TBits n, TBits m -> { e=e; t=TBits (n + m) }
      | TRecord _, TBits _ 
      | TBits _, TRecord _
      | TRecord _, TRecord _ -> failwith "can not concat records"
    end

  | Record r -> 
    (* TODO the field names must be unique *)
    let fields = List.map fst r in
    let values = List.map snd r in
    let inferred = List.map (fun e -> infer e tenv cenv) values in
    let es = List.map (fun x -> x.t) inferred in
    let tr = List.map2 (fun x y -> (x, y)) fields es in
    { e=e; t=TRecord tr }

  | Access i -> 
    begin match Env.find_opt i tenv with
      | Some t -> { e=e; t=t }
      | None -> failwith ("Unbound variable " ^ i)
    end

  | Member (e, f) -> 
    let te = infer e tenv cenv in
    begin match te.t with
      | TBits _ -> failwith "Bits type has no members"
      | TRecord r -> 
        begin match List.assoc_opt f r with
          | Some t -> { e=e; t=t }
          | None -> failwith ("Unknown field" ^ f)
        end
    end
