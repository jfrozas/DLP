
(* TYPE DEFINITIONS *)

(*
  In types and terms, we add the mentioned in lexer and parser
*)
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

(* List of type variables *)
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

(* List of term variables  *)
type globVar =
  (string * term) list
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;

(* Checks if a type variable X already exists *)
let rec existsTy ctx x = match ctx with
  [] -> false
  | (x1, t1)::h -> if x = x1 then true else existsTy h x
;;

(* Changes the value of a type variable *)
let rec substVarTy ctx x s ctxNew= match ctx with
  [] -> ctxNew
  | (x1, t1)::h -> if x = x1 then
                          if t1 = s then
                                raise Not_found
                          else substVarTy h x s ((x1,s)::ctxNew)
                   else substVarTy h x s ((x1,t1)::ctxNew)
;;



(*globalVar management*)

let emptyGlb =
  []
;;

let getValue glb x =
  List.assoc x glb
;;

let addVar glb x t =
  (x, t) :: glb
;;

(* Checks if a term variable X already exists *)
let rec exists glb x = match glb with
  [] -> false
  | (x1, t1)::h -> if x = x1 then true else exists h x
;;

(* Changes the value of a term variable *)
let rec substVar glb x s glbNew= match glb with
  [] -> glbNew
  | (x1, t1)::h -> if x = x1 then
                          if t1 = s then
                                raise Not_found
                          else substVar h x s ((x1,s)::glbNew)
                   else substVar h x s ((x1,t1)::glbNew)
;;


exception Type_error of string
;;

(* Checks the element on the n-th position, and returns that value. If the n-th element does not exists, returns error *)
let rec aux t n = match t with
    TmAnd (t1, t2) -> if n = 1 then t1 else aux t2 (n-1)
  | t' -> if n = 1 then t' else raise (Type_error "The index is out of bounds")
;;

(* Checks every element on a register, if one of the labels is equal to n, returns the value associated to n, if not, error *)
let rec aux2 t n = match t with
    TmAnd (TmAssign(name, value), t2) -> if name = n then value else aux2 t2 n
  | TmAssign(name, value) -> if name = n then value else raise (Type_error "The label given is not in the register")
  | _ -> raise (Type_error "The elements in the register are wrong")
;;

(* In the register t, gets the associated value for the name n
  If t not a register, Type_error *)
let getRegister t n = match t with
   TmRegister t1 -> aux2 t1 n
   | _ -> raise (Type_error "The first element has to be a register")
;;

(* In the tuple t, gets the element in the n-th position. If t not a tuple, Type_error  *)
let getTupla t n = match t with
   TmTuple t1 -> aux t1 n
   | _ -> raise (Type_error "The first element has to be a tuple")
;;





(* TYPE MANAGEMENT (TYPING) *)

(* Returns the string of a given type
   We added a pattern-matching case for every new type that we added, such as Variants, strings or tuples*)
let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      string_of_ty ty1 ^ " -> " ^ string_of_ty ty2
  | TyString ->
      "String"
  | TyChar ->
    "Char"
  | TyTuple ty1 ->
      "{" ^ string_of_tyTuple ty1 ^ "}"
  | TmVarTy s ->
      s
  | TyVarElement (s, t) ->
     s ^ ":" ^ string_of_ty t
  | TyVariante s ->
     "<" ^ string_of_ty s ^ ">"
  | TyAnd (t1, t2) ->
     string_of_ty t1 ^ ", " ^ string_of_ty t2

     (* As tuples use arrays, to print them correctly, we use this function *)
and string_of_tyTuple ty = match ty with
    TyArr (ty1, ty2) ->
      string_of_tyTuple ty1 ^ ", " ^ string_of_tyTuple ty2
  | _ -> string_of_ty ty

    (* Gets the type of a variable -> mainly used for type variables *)
  let rec typeofTy ctx ty = match ty with
  TyBool ->
    TyBool
  | TyNat ->
    TyNat
  | TyArr (ty1, ty2) ->
      TyArr (typeofTy ctx ty1, typeofTy ctx ty2)
  | TyString ->
      TyString
  | TyChar ->
      TyChar
  | TyTuple t ->
      TyTuple t
  | TmVarTy s ->
      getbinding ctx s
  | TyVarElement (s, t) ->
      TyVarElement (s, t)
  | TyVariante s ->
      TyVariante s
  | TyAnd (t1, t2) ->
      TyAnd (t1, t2)
;;


(* TERMS MANAGEMENT (EVALUATION) *)

 (* Returns the string of a given term
   We added a pattern-matching case for every new type that we added, such as Variants, strings or tuples

   ---- Pretty printing ----
   The first time we dont put parenthesis, and after, depending on the first term, we call an auxiliar function (string_of_term 2 3 and 4). that puts parenthesis when needed

   *)
let rec string_of_term = function
  TmTrue ->
    "true"
  | TmFalse ->
    "false"
  | TmIf (t1,t2,t3) ->
    "if " ^ string_of_term2 t1  ^
    " then " ^ string_of_term2 t2  ^
    " else " ^ string_of_term2 t3
  | TmZero ->
    "0"
  | TmSucc t ->
  let rec f n t' = match t' with
        TmZero -> string_of_int n
      | TmSucc s -> f (n+1) s
      | _ -> "succ " ^ string_of_term3 t
    in f 1 t
  | TmPred t ->
    "(" ^"pred " ^ string_of_term4 t ^ ")"
  | TmIsZero t ->
    "iszero " ^ string_of_term4 t
  | TmVar s ->
    s
  | TmAbs (s, tyS, t) ->
    "lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term2 t
  | TmApp (t1, t2) ->
    string_of_term4 t1  ^ " " ^ string_of_term4 t2
  | TmLetIn (s, t1, t2) ->
    "let " ^ s ^ " = " ^ string_of_term4 t1  ^ " in " ^ string_of_term4 t2
  | TmFix t ->
    "fix " ^  string_of_term4 t
  | TmString s ->
    "\"" ^ s ^ "\""
  | TmChar s ->
    "\'" ^ String.make 1 s ^ "\'"
  | TmConcat (t1, t2) ->
    "concat " ^ string_of_term4 t1 ^ " " ^ string_of_term4 t2
  | TmLength s ->
    "length " ^ string_of_term4 s
  | TmGet (t1, t2) ->
    "get " ^ string_of_term4 t1 ^ " " ^ string_of_term4 t2
  | TmAssign (s, t) ->
    s ^ " = " ^ string_of_term t
  | TmAssignTy (s, t) ->
    s ^ " = " ^ string_of_ty t
  | TmTuple t ->
    "{" ^ string_of_term t ^ "}"
  | TmRegister t ->
    "{" ^ string_of_term t ^ "}"
  | TmAnd (t1, t2) ->
    string_of_term2 t1 ^ ", " ^ string_of_term2 t2
  | TmGetTupla (t1, t2) ->
    string_of_term2 t1 ^ "." ^ string_of_term2 t2
  | TmVarianteElement (s, t) ->
    "<" ^ s  ^ " = " ^ string_of_term2 t ^ ">"
  | TmVariante (t1, t2) ->
    string_of_ty t2 ^ " = " ^ string_of_term t1
  | TmCase (t1, t2) ->
    "case " ^ string_of_term t1 ^ " of " ^ string_of_term t2
  | TmOptions (t1, t2) ->
    string_of_term t1 ^ "\n| " ^ string_of_term t2
  | TmOption (t1, t2) ->
    string_of_term t1 ^ " => " ^ string_of_term t2

  (* Only parenthesis if what comes next is a TmApp *)
and string_of_term2 t = (match t with
  TmApp (t1, t2) -> "(" ^ string_of_term4 t1  ^ " " ^ string_of_term4 t2 ^ ")"
  |_ -> string_of_term t )

  (* Always parenthesis *)
and string_of_term3 t = (match t with
  TmPred t1 -> string_of_term t
  |_ ->  "(" ^ string_of_term t ^ ")" )

  (* Only parenthesis if what is coming is not a "simple" statement (such as variable, char ...) *)
and string_of_term4 t = (match t with
  TmVar s -> s
  | TmString s->
    "\"" ^ s ^ "\""
  | TmChar s->
    "\'" ^ String.make 1 s ^ "\'"
  |TmTrue -> "true"
  |TmFalse -> "false"
  |TmZero -> "0"
  |TmPred t1 -> string_of_term t
  |_ -> "(" ^ string_of_term t  ^ ")")




exception NoRuleApplies
;;

(* Returns the number of a numerical value, n=0  in the first call
   Firstly added for the function get of strings *)
let rec getNumber n t = match t with
   TmZero -> n
  | TmSucc s -> getNumber (n+1) s
  | TmPred s -> getNumber (n-1) s
  | _ -> raise NoRuleApplies
;;


let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;


let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;


let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

(* Given function, we added some changes *)
let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
    (* Gets the free variables of t *)
  | TmFix t ->
      free_vars t
    (* Returns an empty list for strings and chars *)
  | TmString _ ->
      []
  | TmChar _ ->
    []
    (* Returns the union of the free_variables of t1 and t2 *)
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
    (* Gets the free variables of t *)
  | TmLength t ->
    free_vars t
    (* Returns the union of the free_variables of t1 and t2 *)
  | TmGet (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
    (* Gets the free variables of t *)
  | TmAssign (s, t) ->
      free_vars t
  | TmAssignTy (s, t) ->
      []
    (* Gets the free variables of t *)
  | TmTuple t ->
    free_vars t
    (* Gets the free variables of t *)
  | TmRegister t ->
    free_vars t
    (* Returns the union of the free_variables of t1 and t2 *)
  | TmAnd (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
    (* Returns the union of the free_variables of t1 and t2 *)
  | TmGetTupla (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
    (* Gets the free variables of t *)
  | TmVarianteElement (s, t) ->
    free_vars t
    (* Gets the free variables of t1 *)
  | TmVariante (t1, t2) ->
    free_vars t1
    (* Returns the union of the free_variables of t1 and t2 *)
  | TmCase (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
    (* Returns the union of the free_variables of t1 and t2 *)
  | TmOptions (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
    (* Returns the union of the free_variables of t1 and t2 *)
  | TmOption (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
 ;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

(* Substitues the variable x for the term s in the term tm
   Given function, we added some changes *)
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  (* Substitutes in the term t *)
  | TmFix t ->
     TmFix (subst x s t)
  (* In string and char, we dont modify anything *)
  | TmString  st ->
      TmString st
  | TmChar  st ->
      TmChar st
  (* Substitutes in the terms t1 and t2 *)
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  (* Substitutes in the term t *)
  | TmLength t ->
    TmLength (subst x s t)
  (* Substitutes in the terms t1 and t2 *)
  | TmGet (t1, t2) ->
    TmGet (subst x s t1, subst x s t2)
  (* Substitutes in the term t *)
  | TmAssign (s1, t) ->
      TmAssign (s1, subst x s t)
  (* We dont modify anything *)
  | TmAssignTy (s1, t) ->
      TmAssignTy (s1, t)
  (* Substitutes in the term t *)
  | TmTuple t ->
      TmTuple (subst x s t)
  (* Substitutes in the term t *)
  | TmRegister t ->
      TmRegister (subst x s t)
  (* Substitutes in the terms t1 and t2 *)
  | TmAnd (t1, t2) ->
      TmAnd (subst x s t1, subst x s t2)
  (* Substitutes in the terms t1 and t2 *)
  | TmGetTupla (t1, t2) ->
      TmGetTupla (subst x s t1, subst x s t2)
  (* Substitutes in the term t *)
  | TmVarianteElement (st, t) ->
    TmVarianteElement (st, subst x s t)
  (* Substitutes in the terms t1 and t2 *)
  | TmVariante (t1, t2) ->
      TmVariante (subst x s t1, t2)
  (* Substitutes in the terms t1 and t2 *)
  | TmCase (t1, t2) ->
    TmCase (subst x s t1, subst x s t2)
  (* Substitutes in the terms t1 and t2 *)
  | TmOption (t1, t2) ->
    TmOption (subst x s t1, subst x s t2)
  (* Substitutes in the terms t1 and t2 *)
  | TmOptions (t1, t2) ->
    TmOptions (subst x s t1, subst x s t2)
;;

(*  *)
let rec auxVariantes ty ctx = match ty with
    TyAnd (t1, t2) -> auxVariantes t2 (auxVariantes t1 ctx)
  | TyVarElement (t1, t2) -> addbinding ctx t1 t2
  | _ -> raise (Type_error "The variant type isnt well formed")
;;

(* Stores the constructors of a type variant as a type variable, in order to be able to know the type of an element in a tmVarElement *)
(* :D *)
let addVariantes (TyVariante ty) ctx =
    auxVariantes ty ctx
;;

(* Only added the tmVariante and tmVarianteElement *)
let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmChar _ -> true
  | TmVariante _ -> true
  | TmVarianteElement _ -> true
  | t when isnumericval t -> true
  | _ -> false
;;

(* Principal evaluation function *)

let rec eval1 tm glb ctx = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2, glb, ctx

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3, glb, ctx

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1', glb1, ctx1 = eval1 t1 glb ctx in
      TmIf (t1', t2, t3), glb1, ctx1

    (* E-Succ *)
  | TmSucc t1 ->
      let t1', glb1, ctx1 = eval1 t1 glb ctx in
      TmSucc t1', glb1, ctx1

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero, glb, ctx

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1, glb, ctx

    (* E-Pred *)
  | TmPred t1 ->
      let t1', glb1, ctx1 = eval1 t1 glb ctx in
      TmPred t1', glb1, ctx1

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue, glb, ctx

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse, glb, ctx

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1', glb1, ctx1 = eval1 t1 glb ctx in
      TmIsZero t1', glb1, ctx1

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12, glb, ctx

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2', glb1, ctx1 = eval1 t2 glb ctx in
      TmApp (v1, t2'), glb1, ctx1

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1', glb1, ctx1 = eval1 t1 glb ctx in
      TmApp (t1', t2), glb1, ctx1

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2, glb, ctx

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1', glb1, ctx1 = eval1 t1 glb ctx in
      TmLetIn (x, t1', t2), glb1, ctx1

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2, glb, ctx
    (* E-Fix *)
  | TmFix t1 ->
      let t1', glb1, ctx1 = eval1 t1 glb ctx in
      TmFix t1' , glb1, ctx1
    (* If t1 and t2 are strings, we concant both strings *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2), glb, ctx
    (* If t1 is string, we eval t2 *)
  | TmConcat (TmString s1, t2) ->
      let t2', glb1, ctx1 = eval1 t2 glb ctx in
      TmConcat (TmString s1, t2'), glb1, ctx1
    (* Eval t1 *)
  | TmConcat (t1, t2) ->
      let t1', glb1, ctx1 = eval1 t1 glb ctx in
      TmConcat (t1', t2), glb1, ctx1
    (* If s is a string, we get its length by using string.length and returns that value as a term *)
  | TmLength (TmString s) ->
    let rec f = function
       0 -> TmZero
      | n -> TmSucc (f (n-1))
    in f (String.length s), glb, ctx
    (* Eval s *)
  | TmLength s ->
    let t, glb1, ctx1 = eval1 s glb ctx in
    TmLength t, glb1, ctx1
    (* If s1 is a string, we check if n is a numerical value, if not, we eval n, else, we transform n into a Nat using getNumber and we call String.get, which will return the char at a given position, and we store the result in a TmChar*)
  | TmGet (TmString s1, n) ->
    if isnumericval n then TmChar (String.get s1 (getNumber 0 n)), glb, ctx
    else let t, glb1, ctx1 = eval1 n glb ctx in TmGet (TmString s1,t), glb1, ctx1
    (* Eval t1 *)
  | TmGet (t1, t2) ->
    let t1', glb1, ctx1 = eval1 t1 glb ctx in
    TmGet (t1', t2), glb1, ctx1
    (* We first check if s exists
        -If it exists, we try to substitute its value for the new value t
          -If when substituting retuns not_found, this says that it already has that value -> NoRuleApplies
        -If it doesnt exists, we eval the term t, and add the variable with the evaluated term
          -If when evaluating the term "NoRuleApplies" is raised, this says that the term is already evaluated, so we just add the variable with the term without evaluate
      *)
  | TmAssign (s, t) ->
      (if exists glb s then
          try
            let c = substVar glb s t emptyGlb in TmAssign(s,t), c, ctx
          with
            Not_found -> raise NoRuleApplies
       else
        try
          (let t', glb1, ctx1 = eval1 t glb ctx in let a = addVar glb1 s t' in TmAssign (s, t'), a, ctx1)
        with
          NoRuleApplies -> let a = addVar glb s t in TmAssign (s, t), a, ctx)
      (* Special case for when the new type being created is a variant type 
        We do the same as the case below, but we call the funcion addVariantes (Knowing the type, explained above)    
      *)

  | TmAssignTy (s, TyVariante (ty)) ->
    (let ctx1 = addVariantes (TyVariante (ty)) ctx in
        if existsTy ctx1 s then
          try
            let c = substVarTy ctx1 s ty emptyctx in TmAssignTy(s,ty), glb, c
          with
            Not_found -> raise NoRuleApplies
        else
          let ty1 = typeofTy ctx1 ty in let a = addbinding ctx1 s ty1 in TmAssignTy (s, ty1), glb, a)
      (* We first check if s exists
          -If exists, we substitute
            -If when substituting Not_found is raised, we dont do anything
          -If it doesnt exist, we add the type 
      *)
  | TmAssignTy (s, ty) ->
    (if existsTy ctx s then
          try
            let c = substVarTy ctx s ty emptyctx in TmAssignTy(s,ty), glb, c
          with
            Not_found -> raise NoRuleApplies
        else
          let ty1 = typeofTy ctx ty in let a = addbinding ctx s ty1 in TmAssignTy (s, ty1), glb, a)
      (* Trys to get the value of that variable, if not possible, return NoRuleApplies *)
  | TmVar s ->(
    try
      getValue glb s, glb, ctx
    with
      Not_found -> raise NoRuleApplies)

      (* Evals t -> If t has a variable, we have to eval t to substitute this variable for its value  *)
  | TmAbs (s, ty, t) ->
    let t', glb1, ctx1 = eval1 t glb ctx in TmAbs (s, ty, t'), glb1, ctx1
      (* Evals t -> t will be a TmAnd or a term *)
  | TmTuple t ->
    let t', glb1, ctx1 = eval1 t glb ctx in TmTuple t', glb1, ctx1
      (* Spetial case -> if there's only one element inside the register, we eval its value *)
  | TmRegister (TmAssign(name, value)) ->
    let t', glb1, ctx1 = eval1 value glb ctx in TmRegister (TmAssign(name, t')), glb1, ctx1
      (* Eval t *)
  | TmRegister t ->
    let t', glb1, ctx1 = eval1 t glb ctx in TmRegister t', glb1, ctx1
      (* Last and of the register
         We eval the value, and if NoRuleApplies, we eval value2 *)
  | TmAnd (TmAssign(name, value), TmAssign(name2, value2)) ->
    (try
      let t1', glb1, ctx1 = eval1 value glb ctx in TmAnd (TmAssign(name, t1'), TmAssign(name2, value2)), glb1, ctx1
    with
      NoRuleApplies -> let t2', glb1, ctx1 = eval1 value2 glb ctx in TmAnd (TmAssign(name, value), TmAssign(name2, t2')), glb1, ctx1 )
      (* t2 is an and, and we are evaluating a Register
         We eval the value, and if NoRuleApplies, we eval t2 *)
  | TmAnd (TmAssign(name, value), t2) ->
    (try
      let t1', glb1, ctx1 = eval1 value glb ctx in TmAnd (TmAssign(name, t1'), t2), glb1, ctx1
    with
      NoRuleApplies -> let t2', glb1, ctx1 = eval1 t2 glb ctx in TmAnd (TmAssign(name, value), t2'), glb1, ctx1 )
      (* We eval t1, and if NoRuleApplies, we eval t2
         NoRulesApplies means that its an and of a tuple, the we evaluate the righ one  
      *)
  | TmAnd (t1, t2) ->
    (try
      let t1', glb1, ctx1 = eval1 t1 glb ctx in TmAnd (t1', t2), glb1, ctx1
    with
      NoRuleApplies -> let t2', glb1, ctx1 = eval1 t2 glb ctx in TmAnd (t1, t2'), glb1, ctx1 )
    (* If tuple, we check if n is a numerical value
        -If it is, we call the function getTupla
        -Else, we evaluate n 
      *)
  | TmGetTupla (TmTuple s1, n) ->
    if isnumericval n then getTupla (TmTuple s1) (getNumber 0 n), glb, ctx
    else let t, glb1, ctx1 = eval1 n glb ctx in TmGetTupla (TmTuple s1,t), glb1, ctx1
    (* If register, we call getRegister *)
  | TmGetTupla (TmRegister s1, n) ->
    getRegister (TmRegister s1) (string_of_term n), glb, ctx
    (* We eval t1 -> can be register or tuple *)
  | TmGetTupla (t1, t2) ->
    let t1', glb1, ctx1 = eval1 t1 glb ctx in
    TmGetTupla (t1', t2), glb1, ctx1
    (* Eval t *)
  | TmVarianteElement (s, t) ->
    let t', glb1, ctx1 = eval1 t glb ctx in TmVarianteElement (s, t'), glb1, ctx1
    (* Eval t1 *)
  | TmVariante (t1, t2) ->
      let t1', glb1, ctx1 = eval1 t1 glb ctx in TmVariante (t1', t2), glb1, ctx1

    (* Sorry for this function :/ *)
    (*     
      p3 = <pos=3> as Int;;

      abs = L i : Int.
          case i of 
            <pos=p> => (<pos=p> as Int)
          | <zero=z> => (<zero=true> as Int)
          | <neg=n> => (<pos=n> as Int)
      ;;

      abs p3;;

      i = p3
      
      we loop throgh the option
      In each case, we check that pos=s, s being <s = x> (anything)
      If one of them matches, we substitute x for 3 (from <pos = 3> )  in the right hand side of the => 

      => (<pos=3> as Int)

      Else, if no match is found, NoRuleApplies is raised
    *)
  | TmCase (TmVariante (TmVarianteElement (s, t), _), cases) ->
    (* ------evalOption---- 
    The parameters are a tmVariantElement and tmOption that has a tmVariantElement and a term t2'
        We check if both strings of tmVariantElement are the same.
          -If they are, we return True, the value of the second TmVarianteElement (that is the name of the variable of the pattern-matching), the value of the first TmVarianteElement (that is the value we are making pattern-matching with) and the righ hand side term of the pattern-matching
          -Else, we return the same, except for the first value, that is false
    *)
    (let evalOption (TmVarianteElement (s1, t1)) (TmOption (TmVarianteElement (s2, t'), t2')) =
      if s1 = s2 then true, t', t1, t2' else false, t', t1, t2'
    (* 
      aux
      Parameters: tmVarianteElement and the Options of TmCase
      It does pattern-matching with the Options in TmOptions and TmOption (more than 1 or only 1)
      -It checks with the EvalOption if the option matches the pattern-matching
        -If it is, it evals the righ-hand side part of the option substituting the variable for the value
        -If not, continues looping through the options, when reaching the last one, NoRuleApplies is raised
    *)
    in let rec aux element options = match options with
        TmOptions (TmOption(TmVarianteElement (s2, t2), t2'), option2) -> let yes, localVar, original, rightTerm = evalOption element (TmOption(TmVarianteElement (s2, t2), t2')) in if yes = true then eval (subst (string_of_term localVar) original rightTerm) glb ctx else aux element option2
      | TmOption (TmVarianteElement (s2, t2), t2') -> let yes, localVar, original, rightTerm = evalOption element (TmOption(TmVarianteElement (s2, t2), t2')) in if yes then eval (subst (string_of_term localVar) original rightTerm) glb ctx else raise NoRuleApplies
      | _ -> raise NoRuleApplies
    in aux (TmVarianteElement (s, t)) cases)
    (* Eval t1 *)
  | TmCase (t1, t2) ->
    let t1', glb1, ctx1 = eval1 t1 glb ctx in TmCase (t1', t2), glb1, ctx1

  | _ ->
      raise NoRuleApplies
(* Calls eval1 *)
and eval tm glb ctx=
  try
    let tm', glb1, ctx1 = eval1 tm glb ctx in
    eval tm' glb1 ctx1
  with
    NoRuleApplies -> tm, glb, ctx
;;


(* Returns the type of a term *)
let rec typeof ctx tm glb = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 glb = TyBool then
        let tyT2 = typeof ctx t2 glb in
        if typeof ctx t3 glb = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 glb = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 glb = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 glb = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try let a = getValue glb x in typeof ctx a glb with
        Not_found -> (try
                        getbinding ctx x
                      with
                        Not_found -> raise (Type_error ("no binding type for variable " ^ x))))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 glb in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 glb in
      let tyT2 = typeof ctx t2 glb in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 glb in
      let ctx' = addbinding ctx x tyT1 in
      typeof ctx' t2 glb
    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 glb in
      (match tyT1 with
        TyArr (tyT11, tyT12) ->
          if tyT11 = tyT12 then tyT12
          else raise (Type_error "result of body not compativle with domain")
      | _ -> raise (Type_error "arrow type expected"))
    (* Returns type string *)
  | TmString _ ->
      TyString
    (* Returns type char *)
  | TmChar _ ->
    TyChar
    (* Checks if both t1 and t2 are strings, if true, returns type string, else returns type error *)
  | TmConcat (t1, t2) ->
      if typeof ctx t1 glb = TyString && typeof ctx t2 glb = TyString then TyString
      else raise (Type_error "argument of concat is not a string")
    (* Checks if the arguments passed to length is a string, if true, returns TyNat, else, returns TypeError *)
  | TmLength t ->
    if typeof ctx t glb = TyString then TyNat
    else raise (Type_error "argument of length is not a string")
    (* Checks if t1 is a string, if t2 is a Nat, and returns a char, else, returns TypeError*)
  | TmGet (t1, t2) ->
    if typeof ctx t1 glb = TyString then
      if typeof ctx t2 glb = TyNat then TyChar
      else raise (Type_error "second argument of get is not a nat")
    else  raise (Type_error "first argument of get is not a string")
    (* Type of t *)
  | TmAssign (s, t) ->
      typeof ctx t glb
    (* t *)
  | TmAssignTy (s, t) ->
      t
    (* Returns TyTuple of typeOf s *)
  | TmTuple s ->
    TyTuple (typeof ctx s glb)
    (* Returns TyTuple of typeOf s *)
  | TmRegister s ->
    TyTuple (typeof ctx s glb)
    (* Returns an array of typeOf t1 and Typeof t2 *)
  | TmAnd (t1, t2) ->
    TyArr(typeof ctx t1 glb, typeof ctx t2 glb)

    (* If t1 is a tuple, we check if t2 is a number. 
    -If not, we eval t2.Then, we get the value in the position t2 of t1 and we return the type of that value. 
    -If yes, we get the value in the position t2 of t1 and we return the type of that value 
    If when executing any of these things we are returned with the error not_found or NoRuleApplies, we return TypeError
    *)
  | TmGetTupla (TmTuple t1, t2) ->
    (try
      if isnumericval t2
        then let a = getTupla (TmTuple t1) (getNumber 0 t2) in typeof ctx a glb
      else let t2', glb1, ctx1 = eval t2 glb ctx in let a = getTupla (TmTuple t1) (getNumber 0 t2') in typeof ctx1 a glb1
    with
      Not_found -> raise (Type_error "There is no element in the given position")
      |NoRuleApplies -> raise (Type_error "The index has to be an int"))

    (* If t1 is a register, returns the type of the value associated to the given label t2 *)
  | TmGetTupla (TmRegister t1, t2) ->
    let a = getRegister (TmRegister t1) (string_of_term t2) in typeof ctx a glb
    (* Eval t1 *)
  | TmGetTupla (t1, t2) ->
    let t1', glb1, ctx1 = eval t1 glb ctx in  typeof ctx1 (TmGetTupla (t1', t2)) glb1
    (* Returns typeOf t *)
  | TmVarianteElement(s,t) ->
      typeof ctx t glb
    (* Returns typeOf t *)
  | TmVariante (TmVarianteElement (s, t1), t2) ->
    t2
    (* Returns TypeError (t1 not a tmVarianteElement)  *)
  | TmVariante (t1, t2) ->
      raise (Type_error "The variant type is not well formed")
    (* TypeOf t2 *)
  | TmCase (t1, t2) ->
    typeof ctx t2 glb
    (* We add the type variable t1 with value of the type variable s, and returns the type of t2*)
  | TmOption (TmVarianteElement(s, t1), t2) ->
    let ctx1 = addbinding ctx (string_of_term t1) (getbinding ctx s) in
    typeof ctx1 t2 glb
    (* Returns TypeError (t1 not a tmVarianteElement)  *)
  | TmOption (t1, t2) -> 
     raise (Type_error "The case of is not well formed")
    (* Youc check that the types of both t1 and t2 are the same, if true, return that type, if not, you raise type_error *)
  | TmOptions (t1, t2) ->
    let t = typeof ctx t1 glb in
    if t = typeof ctx t2 glb then t else  raise (Type_error "all the cases have to have the same type")

  ;;
