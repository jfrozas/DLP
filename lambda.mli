
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyChar
  | TyTuple of ty
  | TmVarTy of string
  | TyVarElement of string * ty
  | TyVariante of ty
  | TyAnd of ty * ty
;;

type context =
  (string * ty) list
;;


type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmChar of char
  | TmConcat of term * term
  | TmLength of term
  | TmGet of term * term
  | TmAssign of string * term
  | TmAssignTy of string * ty
  | TmAnd of term * term
  | TmTuple of term
  | TmGetTupla of term * term
  | TmRegister of term
  | TmVarianteElement of string * term
  | TmVariante of term * ty
  | TmCase of term * term
  | TmOptions of term * term
  | TmOption of term * term
;;

type globVar =
  (string * term) list
;;

val emptyctx : context;;
val addbinding : context -> string -> ty -> context;;
val getbinding : context -> string -> ty;;
val existsTy : context -> string -> bool;;
val substVarTy : context -> string -> ty -> context -> context;;

val emptyGlb : globVar;;
val addVar : globVar -> string -> term -> globVar;;
val getValue : globVar -> string -> term;;
val existsTy : globVar -> string -> bool;;
val substVarTy : globVar -> string -> term -> globVar -> globVar;;


val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> globVar -> ty;;
val typeofTy : context -> ty -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : term -> globVar -> context -> term * globVar * context;;

