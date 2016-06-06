open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies
exception NoSuchVariable
exception TypeError

let rec find_list l x = match l with
    [] -> None
  | (key, value)::rest -> if x = key then Some value else find_list rest x

let rec rcd_subtypeof(rcd1, rcd2) = match rcd2 with
    [] -> true
  | (key, ty)::rest ->
    (match find_list rcd1 key with
      Some ty' -> (subtypeof ty' ty) && rcd_subtypeof(rcd1, rest)
    | None -> false)

and subtypeof ty1 ty2 =
    if ty1 = ty2 then true else
    match ty1, ty2 with
      TyRcd(rcd1), TyRcd(rcd2) -> rcd_subtypeof(rcd1, rcd2)
    | TyArr(ty11,ty12), TyArr(ty21,ty22) -> (subtypeof ty21 ty11) && (subtypeof ty12 ty22)
    | _ -> false

let rec typeof t ctx = match t with
    TmTrue(fi) ->
      TyBool
  | TmFalse(fi) ->
      TyBool
  | TmIf(fi,t1,t2,t3) when typeof t1 ctx = TyBool ->
      let ty2 = typeof t2 ctx in
      if ty2 = typeof t3 ctx then ty2 else raise TypeError
  | TmZero(fi) ->
      TyNat
  | TmSucc(fi,t1) when typeof t1 ctx = TyNat ->
      TyNat
  | TmPred(fi,t1) when typeof t1 ctx = TyNat ->
      TyNat
  | TmIsZero(fi,t1) when typeof t1 ctx = TyNat ->
      TyBool
  | TmAbs(fi,x,typ,t1) ->
      TyArr(typ, typeof t1 ((x,typ)::ctx))
  | TmApp(fi,t1,t2) ->
      let ty1 = typeof t1 ctx in
      let ty2 = typeof t2 ctx in
      (match ty1 with
        TyArr(ty1',ty2') ->
          if subtypeof ty2 ty1' then ty2' else raise TypeError
        | _ -> raise TypeError)
  | TmVar(fi,x) ->
      let tyx = find_list ctx x in
      (match tyx with
        Some tyx' -> tyx'
      | None -> raise TypeError)
  | TmRcd(fi,rcd) ->
      let rec type_rcd rcd = match rcd with
        [] -> []
      | (x,term)::rest ->
          (x,typeof term ctx)::(type_rcd rest)
      in TyRcd(type_rcd rcd)
  | TmProj(fi,t1,x) ->
      (match typeof t1 ctx with
        TyRcd(rcd) ->
          (match find_list rcd x with
            Some ty1' -> ty1'
          | None -> raise TypeError)
        | _ -> raise TypeError)
  | _ -> raise TypeError

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isrcdval t = match t with
    [] -> true
  | (_,x)::t' -> (isval x) && (isrcdval t')

and isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmAbs (_,_,_,_) -> true
  | TmRcd (_,rcd) -> isrcdval rcd
  | t when isnumericval t  -> true
  | _ -> false

let rec subtitude x v t = match t with
    TmIf(fi,t1,t2,t3) ->
      TmIf(fi, subtitude x v t1, subtitude x v t2, subtitude x v t3)
  | TmSucc(fi,t1) ->
      TmSucc(fi, subtitude x v t1)
  | TmPred(fi,t1) ->
      TmPred(fi, subtitude x v t1)
  | TmIsZero(fi,t1) ->
      TmIsZero(fi, subtitude x v t1)
  | TmAbs(fi,y,typ,t1) when x <> y ->
      TmAbs(fi, y, typ, subtitude x v t1)
  | TmApp(fi,t1,t2) ->
      TmApp(fi, subtitude x v t1, subtitude x v t2)
  | TmVar(fi,y) when x = y ->
      v
  | TmRcd(fi,rcd) ->
      let rec sub_rcd rcd = match rcd with
        [] -> []
      | (key,term)::rest ->
        (key, subtitude x v term)::(sub_rcd rest)
      in TmRcd(fi, sub_rcd rcd)
  | TmProj(fi,t1,name) ->
      TmProj(fi, subtitude x v t1, name)
  | _ ->
      t

let rec eval1 t = match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1) ->
      let t1' = eval1 t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')
  | TmApp(fi,TmAbs(_,x,typ,t12),v2) when (isval v2) ->
      subtitude x v2 t12
  | TmApp(fi,v1,t2) when (isval v1) ->
      let t2' = eval1 t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 t1 in
      TmApp(fi, t1', t2)
  | TmProj(fi, TmRcd(_,rcd), x) ->
      (match find_list rcd x with
        Some term -> term
      | None -> raise NoRuleApplies)
  | TmProj(fi, t1, x) ->
      let t1' = eval1 t1 in
      TmProj(fi, t1', x)
  | TmRcd(fi, rcd) ->
      let rec eval_rcd rcd = match rcd with
        [] ->
        raise NoRuleApplies
      | (x,value)::rest when isval value ->
        (x,value)::(eval_rcd rest)
      | (x,term)::rest ->
        (x,eval1 term)::rest
      in TmRcd(fi,eval_rcd rcd)
  | TmVar(_,_) ->
      raise NoSuchVariable
  | _ -> 
      raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t


