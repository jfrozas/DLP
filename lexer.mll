
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    (* Tokens added:
        - letrec - 3.2.1 - Recursion
        - fix - 3.2.1 - Recursion
        - concat - 3.2.3 - string
        - length - 3.2.3 - string
        - get - 3.2.3 - string
        - string - 3.2.3 - string
        - char - 3.2.3 - string
        - as - 3.2.7 - variants
        - case - 3.2.7 - variants
        - of - 3.2.7 - variants
        - | - 3.2.7 - variants
        - { } - 3.2.4 - tuples
        - < > - 3.2.7 - variants
        - , - 3.2.4 - tuples
        - . - 3.2.4 - tuples
        - => - 3.2.7 - variants
        - ['A'-'Z']['a'-'z' '_' '0'-'9' 'A'-'Z']* IDVTY - 3.2.2 - global definitions
        - '"'[^ '"' ';' '\n']*'"' - STRINGV - 3.2.3 - string
        - '''[^ ''' ';' '\n']*''' - CHARV - 3.2.3 - string
     *)
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "fix"       { FIX }
  | "in"        { IN }
  | "concat"    { CONCAT }
  | "length"    { LENGTH }
  | "get"       { GET }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "Char"      { CHAR }
  | "as"        { AS }
  | "case"      { CASE }
  | "of"        { OF }
  | "|"         { OPTION }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LCORCHETE }
  | '}'         { RCORCHETE }
  | '<'         { LARROW }
  | '>'         { RARROW }
  | ','         { OTHER }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "=>"        { ARROW2 }
  | "->"        { ARROW }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9' 'A'-'Z']*
                { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'Z']['a'-'z' '_' '0'-'9' 'A'-'Z']*
                { IDVTY (Lexing.lexeme lexbuf) }
  | '"'[^ '"' ';' '\n']*'"'
                { let s = Lexing.lexeme lexbuf in STRINGV (String.sub s 1 (String.length s - 2)) }
  | '''[^ ''' ';' '\n']*'''
                { let s = Lexing.lexeme lexbuf in CHARV (String.get s 1) }
  | eof         { EOF }
  | _           { raise Lexical_error }

