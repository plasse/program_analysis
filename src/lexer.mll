{
  open Parser
}

let num = ['0'] | ['1'-'9']['0'-'9']*
let id  = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '-' '_']*
let blank = [' ' '\t']
let newline = ['\n' '\r']

rule token = parse
  | blank     { token lexbuf }
  | newline   { token lexbuf }
  | ":="      { COLONEQ }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "fi"      { FI }
  | "skip"    { SKIP }
  | "while"   { WHILE }
  | "do"      { DO }
  | "done"    { DONE }
  | "source"  { SOURCE }
  | "sink"    { SINK }
  | "filter"  { FILTER }
  | ";"       { SEMI }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { MULTI }
  | "/"       { DIV }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "print"   { PRINT }
  | "&&"      { AND }
  | "||"      { OR }
  | "="       { EQ }
  | "!="      { NEQ }
  | "<"       { LT }
  | ">"       { GT }
  | "<="      { LE }
  | ">="      { GE }
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "!"       { BANG }
  | num  as n { INT (int_of_string n) }
  | id   as s { VAR s }
  | eof       { EOF }

