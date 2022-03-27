open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = parser.expr lexer.token lexbuf in
  print_endline (string_of_expr program)
  (* print_endline (string_of_program program) *)
