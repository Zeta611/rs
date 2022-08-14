type expr =
  | Num of int
  | Var of string
  | App of (expr * expr)
  | Abs of (string * expr)
  | Let of (string * expr * expr)
  | Bop of bop
  | Uop of uop

and bop =
  | Plus of (expr * expr)
  | Minus of (expr * expr)
  | Times of (expr * expr)
  | Div of (expr * expr)

and uop = UMinus of expr
