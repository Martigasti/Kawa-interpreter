{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "print",    PRINT;
      "main",     MAIN;
      "true",   TRUE;
      "false",    FALSE;
      "var",    VAR;
      "attribute",    ATTRIBUTE;
      "method",    METHOD;
      "class",    CLASS;
      "new",    NEW;
      "this",   THIS;
      "if",    IF;
      "else",   ELSE;
      "while",    WHILE;
      "return",   RETURN;
      "int",    INT_TYPE;
      "bool",   BOOL_TYPE;
      "void",   VOID_TYPE;
      "extends", EXTENDS;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }
  (* unaire *)
  | "!" { NOT }

  (* binaire *)
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "%" { REM }
  (* Extension égalité structurelle *)
  | "===" { STREQ }
  | "=/=" { STRNEQ }
  
  | "==" { EQ }
  | "!=" { NEQ }
  | "<" { LT }
  | "<=" { LE }
  | ">" { GT }
  | ">=" { GE }
  | "&&" { AND }
  | "||" { OR }


  | "=" { ASSIGN }

  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "." { DOT }
  | "," { COMMA }


  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | "/*" { () }
  | "//" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
