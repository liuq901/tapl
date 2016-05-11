(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val typeof : term -> context -> ty
val eval : term -> term 
