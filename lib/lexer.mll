{
  open Parser
  exception LexError of string
}

let blank = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"

let digit = ['0'-'9']
let number = '-'? digit+

let ulph = ['_' 'a'-'z' 'A'-'Z']
let ulnum = ulph | digit
let var = ulph ulnum*

rule token = parse
| blank
    { token lexbuf }
| newline
    { token lexbuf }
| eof
    { EOF }
| number as n
    { NUM (int_of_string n) }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIV }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '\\'
    { LAMBDA }
| '.'
    { DOT }
| "let"
    { LET }
| '='
    { EQ }
| "in"
    { IN }
| var as x
    { VAR x }
| _
    { raise (LexError (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
