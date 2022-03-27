type op =
  | Add  | Sub   | Mul  | Div  | Flr   | Exp
  | Inc  | Dec   | Mod
  | Eq    | Neq  | Lt   | Gt   | Geq   | Leq
  | LAnd | LOr   | LNot 
  | And  | Or    | Not  | Is   | IsNot 
  | In   | NotIn 

type typ =
  | Char | String | Bool | Int | Float | Void

type bind = typ * string

type expr =
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Call of string * expr list
  | Assign of string * expr
  | Seq of expr * expr

type compound_stmt = int

type simple_stmt =
  | Return of expr
  | Then of expr
  | Import of expr
  | Expr of expr
  | Throw of expr 

type simple_stmts = simple_stmt list

type stmt = 
  | Simple_stmts of simple_stmts
  | Compound_stmt of compound_stmt

type stmts = stmt list

type program = stmts









(* start of printing operations *)

let string_of_op = function
  | Add   -> "+"
  | Sub   -> "-"
  | Mul   -> "*"
  | Div   -> "/"
  | Flr   -> "//"
  | Exp   -> "^"
  | Inc   -> "++"
  | Dec   -> "--"
  | Mod   -> "%"
  | Eq    -> "=="
  | Neq   -> "!="
  | Gt    -> ">"
  | Lt    -> "<"
  | Geq   -> ">="
  | Leq   -> "<="
  | LAnd  -> "&&"
  | LOr   -> "||"
  | LNot  -> "!"
  | And   -> "and"
  | Or    -> "or"
  | Not   -> "not"
  | Is    -> "is"
  | IsNot -> "is not"
  | In    -> "in"
  | NotIn -> "not in"

let rec string_of_expr = function
    IntLit(i) -> string_of_int i
  | FloatLit(f) -> string_of_float f
  | CharLit(c)  -> String.make 1 c
  | StringLit(s) -> s
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Seq(e1, e2) -> (string_of_expr e1) ^ ";" ^ string_of_expr e2

let rec string_of_stmt = function
  Expr(expr) -> string_of_expr expr ^ "\n"

let string_of_typ = function
  Int -> "int"
| Bool -> "bool"
| Char -> "char"
| String -> "string"
| Float -> "float"
| Void -> "void"