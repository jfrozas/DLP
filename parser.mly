
%{
  open Lambda;;
%}

// Added the tokes mentioned in the lexer.mll

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token FIX
%token IN
%token CONCAT
%token LENGTH
%token GET
%token BOOL
%token NAT
%token STRING
%token CHAR
%token OTHER
%token AS
%token CASE
%token OF

%token LPAREN
%token RPAREN
%token LCORCHETE
%token RCORCHETE
%token LARROW
%token RARROW
%token DOT
%token EQ
%token COLON
%token ARROW
%token ARROW2
%token EOF
%token OPTION

%token <int> INTV
%token <string> IDV
%token <string> IDVTY
%token <string> STRINGV
%token <char> CHARV

%start s
%type <Lambda.term> s

%%

s :
    term EOF
      {$1 }

term :
    appTerm
      {$1 }
  | IF term THEN term ELSE term
      {TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      {TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      {TmLetIn ($2, $4, $6) }
      // Internally, letrec works by using letIn(same as let) and fix
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }
      // We use the TmAssign to assign a term to a variable
  | IDV EQ term
      { TmAssign ($1, $3) }
      // We use the TmAssign to assign a type to a variable
  | IDVTY EQ ty
      { TmAssignTy ($1, $3) }
      // TmVariantElement represents one of the contructuring options of a variant type
  | LARROW IDV EQ term RARROW
      { TmVarianteElement ($2, $4) }
      // We use TmVariante when you create a variant 
  | term AS ty
      { TmVariante ($1, $3) }
      // Used to cath case of, whis is, using pattern matching on a variant type
  | CASE term OF cases
      { TmCase ($2, $4) }

// Different pattern-matching options of a case of
cases:
    // Recursion and stores
    term ARROW2 term OPTION cases
        {TmOptions (TmOption ($1, $3), $5)}
    // No recursion and stores
    | term ARROW2 term
        {TmOption ($1, $3)}


appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
    // Returns the concatenation of two terms (if not strings, gives typeError)
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
    // Returns the length of two terms (if not strings, gives typeError)
  | LENGTH atomicTerm
      { TmLength $2 }
    // Returns the char of the string at a position given by parameter
  | GET atomicTerm atomicTerm
      { TmGet ($2, $3) }
    // Get the element of a tuple at a determined position given by parameter $3
  | appTerm DOT atomicTerm
      { TmGetTupla ($1, $3) }
    // Apply AppTerm to atomicTerm
  | appTerm atomicTerm
      { TmApp ($1, $2) }
    // Calls fix of $2
  | FIX atomicTerm
        { TmFix $2 }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
    // If there are brackets, and inside a variable equals something, then we store that as a register, and if not, we store that as a tuple
    // More than 1 element in register
  | LCORCHETE IDV EQ term OTHER tupla RCORCHETE
      { TmRegister (TmAnd (TmAssign ($2, $4), $6)) }
    // 1 element in register
  | LCORCHETE IDV EQ term RCORCHETE
      { TmRegister (TmAssign ($2, $4)) }
    // Tuple
  | LCORCHETE tupla RCORCHETE
      { TmTuple $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
    // Creates a string
  | STRINGV 
      { TmString $1 }
    // Creates a char
  | CHARV 
      { TmChar $1 }


tupla :
    // Recursion of registers and tuples
    term OTHER tupla
        {TmAnd ($1, $3)}
    | term
        {$1}


variante :
    // Recursion for variant type
    // More than 1
    IDV COLON atomicTy OTHER variante
        {TyAnd (TyVarElement ($1, $3), $5)}
    // Only 1
    | IDV COLON atomicTy
        {TyVarElement ($1, $3)}

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
    // Creates a variant type 
  | LARROW variante RARROW
      { TyVariante $2}

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
    // Creates type string
  | STRING
      { TyString }
    // Creates type char
  | CHAR
      { TyChar }
    // Creates type variable
  | IDVTY
      { TmVarTy $1 }

