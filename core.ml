open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies
exception NoSuchVariable

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmAbs (_,_,_) -> true
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
  | TmAbs(fi,y,t1) when x <> y ->
      TmAbs(fi, y, subtitude x v t1)
  | TmApp(fi,t1,t2) ->
      TmApp(fi, subtitude x v t1, subtitude x v t2)
  | TmVar(fi,y) when x = y ->
      v
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
  | TmApp(fi,TmAbs(_,x,t12),v2) when (isval v2) ->
      subtitude x v2 t12
  | TmApp(fi,v1,t2) when (isval v1) ->
      let t2' = eval1 t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 t1 in
      TmApp(fi, t1', t2)
  | TmVar(_,_) ->
      raise NoSuchVariable
  | _ -> 
      raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t


