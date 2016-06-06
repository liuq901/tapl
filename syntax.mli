(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyRcd of (string * ty) list

type context = (string * ty) list

type term =
    TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmVar of info * string
  | TmRcd of info * (string * term) list
  | TmProj of info * term * string

type command =
  | Eval of info * term



(* Printing *)
val printtm: term -> unit
val printtm_ATerm: bool -> term -> unit
val printty: ty -> unit

(* Misc *)
val tmInfo: term -> info

