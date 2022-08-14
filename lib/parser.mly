%{ open Syntax %}


%token <string> VAR
%token <int> NUM
%token LPAREN RPAREN
%token LAMBDA DOT LET EQ IN
%token PLUS MINUS TIMES DIV
%token EOF

%start <expr> program

%%

program:
  | expr EOF
    { $1 }

expr:
  | LET VAR EQ expr IN expr
    { Let ($2, $4, $6) }
  | LAMBDA VAR DOT expr
    { Abs ($2, $4) }
  | bop1
    { $1 }

bop1:
  | bop1 PLUS bop2
    { Bop (Plus ($1, $3)) }
  | bop1 MINUS bop2
    { Bop (Minus ($1, $3)) }
  | bop2
    { $1 }

bop2:
  | bop2 TIMES app
    { Bop (Times ($1, $3)) }
  | bop2 DIV app
    { Bop (Div ($1, $3)) }
  | app
    { $1 }

app:
  | app atom
    { App ($1, $2) }
  | uop
    { $1 }

uop:
  | MINUS uop
    { Uop (UMinus $2) }
  | atom
    { $1 }

atom:
  | NUM
    { Num $1 }
  | VAR
    { Var $1 }
  | LPAREN expr RPAREN
    { $2 }
