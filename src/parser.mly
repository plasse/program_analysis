%{

%}

/* Tokens */

%token COLONEQ IF THEN ELSE FI SKIP
%token WHILE DO DONE SEMI PRINT
%token PLUS MINUS MULTI DIV
%token LPAREN RPAREN
%token EOF
%token<int> INT
%token<string> VAR

%left SEMI PLUS MINUS MULTI DIV

%start s
%type <Playground.s> s

%%

s:
  | VAR COLONEQ e         { Playground.Assign ($1, $3) }
  | IF e THEN s ELSE s FI { Playground.If ($2, $4, $6) }
  | SKIP                  { Playground.Skip }
  | WHILE e DO s DONE     { Playground.While ($2, $4) }
  | s SEMI s              { Playground.Seq ($1, $3) }
  | PRINT e               { Playground.Print $2 }
  ;

e:
  | INT                   { Playground.Int $1 }
  | VAR                   { Playground.Var $1 }
  | e bop e               { Playground.Bop ($2, $1, $3) }
  | LPAREN e RPAREN       { $2 }
  ;

bop:
  | PLUS                  { Playground.Plus }
  | MINUS                 { Playground.Minus }
  | MULTI                 { Playground.Mult }
  | DIV                   { Playground.Div }
  ;
