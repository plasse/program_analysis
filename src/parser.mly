%{
  let lab = ref 0
  let get_lab () =
    incr lab;
    !lab
%}

/* Tokens */

%token COLONEQ IF THEN ELSE FI SKIP
%token WHILE DO DONE SEMI PRINT
%token PLUS MINUS MULTI DIV
%token AND OR XOR EQ NEQ LT GT LE GE TRUE FALSE BANG
%token LPAREN RPAREN
%token EOF
%token<int> INT
%token<string> VAR

%left SEMI PLUS MINUS MULTI DIV
%left AND OR XOR EQ NEQ LT GT LE GE
%right BANG

%start s
%type <Ast.s> s

%%

s:
  | VAR COLONEQ a         { Ast.Assign ($1, $3, get_lab()) }
  | IF b THEN s ELSE s FI { Ast.If ($2, $4, $6, get_lab()) }
  | SKIP                  { Ast.Skip (get_lab()) }
  | WHILE b DO s DONE     { Ast.While ($2, $4, get_lab()) }
  | s SEMI s              { Ast.Seq ($1, $3) }
  | PRINT a               { Ast.Print ($2, get_lab()) }
  ;

a:
  | INT                   { Ast.Int $1 }
  | VAR                   { Ast.Var $1 }
  | a abop a              { Ast.ABop ($2, $1, $3) }
  | LPAREN a RPAREN       { $2 }
  ;

b:
  | TRUE                  { Ast.True }
  | FALSE                 { Ast.False }
  | BANG b                { Ast.Not $2 }
  | b bbop b              { Ast.BBop ($2, $1, $3) }
  | a brop a              { Ast.BRop ($2, $1, $3) }
  ;

abop:
  | PLUS                  { Ast.Plus }
  | MINUS                 { Ast.Minus }
  | MULTI                 { Ast.Mult }
  | DIV                   { Ast.Div }
  ;

bbop:
  | AND                   { Ast.And }
  | OR                    { Ast.Or }
  ;

brop:
  | EQ                    { Ast.Eq }
  | NEQ                   { Ast.Neq }
  | LT                    { Ast.Lt }
  | GT                    { Ast.Gt }
  | LE                    { Ast.Le }
  | GE                    { Ast.Ge }
  ;
