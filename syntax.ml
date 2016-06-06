open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyRcd of (string * ty) list

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

type context = (string * ty) list

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
    TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi,_) -> fi
  | TmPred(fi,_) -> fi
  | TmIsZero(fi,_) -> fi
  | TmAbs(fi,_,_,_) -> fi
  | TmApp(fi,_,_) -> fi
  | TmVar(fi,_) -> fi
  | TmRcd(fi,_) -> fi
  | TmProj(fi,_,_) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let rec printRcdTy t = match t with
    [] -> ()
  | (name, ty) :: t' ->
       pr name;
       pr ":";
       printty ty;
       (match t' with
           [] -> ()
        | _ ->
            pr ", ";
            printRcdTy t')

and printty t = match t with
    TyBool ->
       pr "bool";
  | TyNat ->
       pr "nat";
  | TyArr(t1, t2) ->
       print_ty_par(t1);
       pr "->";
       printty(t2)
  | TyRcd(t1) ->
       pr "{";
       printRcdTy(t1);
       pr "}"

and print_ty_par t = match t with
    TyArr(_,_) ->
       pr "(";
       printty t;
       pr ")"
  | _ ->
       printty t

let rec printtm_Term outer t = match t with
    TmIf(fi, t1, t2, t3) ->
       pr "if ";
       printtm_Term false t1;
       pr " then ";
       printtm_Term false t2;
       pr " else ";
       printtm_Term false t3;
  | TmAbs(fi, t1, typ, t2) -> 
       pr "lambda ";
       pr t1;
       pr ":";
       printty typ;
       pr ". ";
       printtm_Term false t2;
  | t -> printtm_AppTerm outer t

and printtm_AppTerm outer t = match t with
    TmPred(_,t1) ->
       pr "pred "; printtm_ATerm false t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm_ATerm false t1
  | TmApp(fi, t1, t2) ->
       printtm_AppTerm false t1;
       pr " ";
       printtm_ATerm false t2;
  | TmProj(_,t1,x) ->
       printtm_Term false t1;
       pr ".";
       pr x
  | TmRcd(_,t1) ->
       pr "{";
       printRcdTerm t1;
       pr "}"
  | t -> printtm_ATerm outer t

and printtm_ATerm outer t = match t with
    TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmZero(fi) ->
       pr "0"
  | TmSucc(_,t1) ->
     let rec f n t = match t with
         TmZero(_) -> pr (string_of_int n)
       | TmSucc(_,s) -> f (n+1) s
       | _ -> (pr "(succ "; printtm_ATerm false t1; pr ")")
     in f 1 t1
  | TmVar(fi, s) ->
     pr s
  | t -> pr "("; printtm_Term outer t; pr ")"

and printRcdTerm t = match t with
    [] -> ()
  | (name, term) :: t' ->
       pr name;
       pr "=";
       printtm_Term false term;
       (match t' with
           [] -> ()
        | _ ->
            pr ", ";
            printRcdTerm t')

let printtm t = printtm_Term true t 




