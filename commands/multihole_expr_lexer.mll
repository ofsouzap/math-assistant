{
  open! Core
  open Multihole_expr_parser
  exception Lexer_error of string
}

let whitespace = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let variable_name = letter+

rule token = parse
  (* Tokens that might be confused with variable names *)
  | 'e'                                     { E }
  | "pi"                                    { PI }
  | "ln"                                    { LN }
  | variable_name as varname                { VAR varname }

  (* Normal stuff *)
  | int as n                                { INT (int_of_string n) }
  | '_'                                     { HOLE }
  | '+'                                     { PLUS }
  | '-'                                     { MINUS }
  | '*'                                     { TIMES }
  | '/'                                     { DIV }
  | '^'                                     { POW }
  | '('                                     { LPAREN }
  | ')'                                     { RPAREN }

  (* Special stuff *)
  | whitespace                              { token lexbuf }
  | eof                                     { EOF }
  | _ as c                                  { raise (Lexer_error (Printf.sprintf "Unexpected character: %c" c)) }
