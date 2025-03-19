%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI
%token PRINT
%token EOF
%token ADD SUB MUL DIV REM 
%token NOT 
%token TRUE FALSE
%token VAR ATTRIBUTE METHOD CLASS NEW THIS
%token IF ELSE WHILE RETURN
%token INT_TYPE BOOL_TYPE VOID_TYPE
%token EQ NEQ LT LE GT GE AND OR
%token ASSIGN DOT COMMA
%token EXTENDS
(* Extension egalite structurelle *)
%token STREQ
%token STRNEQ


(* Priorités *)
%left OR 
%left AND
%nonassoc EQ NEQ STREQ STRNEQ
%nonassoc LT LE GT GE
%left ADD SUB 
%left MUL DIV REM
%left NOT (* tokens pour operations unaires *) 
%left DOT

%start program
%type <Kawa.program> program
(* Pour extension déclarations en série *)
%type <string list> ident_list
%type <(string * typ) list> var_decl

%%


program:
| globals = global_vars classes=list(class_def) MAIN BEGIN main=list(instruction) END EOF
    { {classes; globals; main} }
;

types:
| INT_TYPE { TInt }
| BOOL_TYPE { TBool }
| VOID_TYPE { TVoid }
| id=IDENT { TClass(id) } (* Types crées pour les class *)

ident_list:
| id=IDENT { [id] }
| id=IDENT COMMA ll=ident_list {id :: ll}
;

var_decl:
| VAR t=types il=ident_list SEMI { 
  List.map (fun x -> (x,t))  il
 }
;

global_vars: (* Flatten pour l'extension des déclarations en série *)
| decls=list(var_decl) { List.flatten decls }
;

attr_decl:
| ATTRIBUTE t=types id=IDENT SEMI { (id, t) }
;

(* Pour les méthodes *)
param_decl:
| t=types id=IDENT { (id, t) }
;

param_list:
| p=param_decl { [p] }
| p=param_decl COMMA ll=param_list {p :: ll}


params_decl:
| { [] } (* vide *)
| pl=param_list { pl }

(* Pour extend, une classe peut avoir ou non un parent *)
extends:  
| {None}
| EXTENDS id=IDENT {Some id}

method_def:
| METHOD t=types id=IDENT LPAR params=params_decl RPAR BEGIN locals=list(var_decl) code=list(instruction) END 
{{ method_name = id; code = code; params = params; locals = List.flatten locals; return = t }}
;

class_def:
| CLASS id=IDENT parent=extends BEGIN attrs=list(attr_decl) meths=list(method_def) END
  { { class_name = id; attributes=attrs; methods=meths; parent = parent } }
;

(* Pour déclarer les attributs d'une classe *)
args:
| { [] }
| e=expression { [e] }
| e=expression COMMA rest=args { e :: rest }
;


instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| mem=mem ASSIGN e=expression SEMI { Set(mem, e) }
| IF LPAR cond=expression RPAR BEGIN instrs=list(instruction) END ELSE BEGIN instrs2=list(instruction) END 
  { If(cond, instrs, instrs2) }
| WHILE LPAR cond=expression RPAR BEGIN instr=list(instruction) END
  { While(cond, instr) }
| RETURN e=expression SEMI { Return(e) } 
| e=expression SEMI { Expr(e) } (* Appeler une fonction *)
;

expression:
| n=INT { Int(n) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| THIS { This }
| LPAR e=expression RPAR { e }
| e1=expression bop=binop e2=expression { Binop(bop, e1, e2) } 
| uop=unop e=expression { Unop(uop, e) } 
| mem=mem { Get(mem) }
| NEW id=IDENT { New(id) }
| NEW id=IDENT LPAR args=args RPAR { NewCstr(id, args) }
| obj=expression DOT id=IDENT LPAR args=args RPAR { MethCall(obj, id, args) }
;


%inline unop:
  | SUB { Opp } (* On utilise le meme SUB mais renvoie autre action pour differencier *)
  | NOT { Not }

%inline binop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | REM { Rem }
  | EQ { Eq }
  | NEQ { Neq }
  | LT { Lt }
  | LE { Le }
  | GT { Gt }
  | GE { Ge }
  | AND { And }
  | OR { Or }
  (* Extension égalité structurelle *)
  | STREQ { StrEq }
  | STRNEQ { StrNeq }


mem:
| id=IDENT { Var(id) } (* Acces a une variable *)
| e=expression DOT id=IDENT { Field(e, id) } (* Acces a l'attribut d'une classe *)
