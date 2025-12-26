{
  open! Core
  open Multihole_expr_parser
  exception Lexer_error of string
}

let whitespace = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let letter_not_lower_e = ['a'-'d' 'f'-'z' 'A'-'Z']
let letter_not_lower_p = ['a'-'o' 'q'-'z' 'A'-'Z']
let letter_not_lower_i = ['a'-'h' 'j'-'z' 'A'-'Z']
let single_letter_variable_name = letter_not_lower_e
let multi_letter_variable_name = letter_not_lower_p letter+ | 'p' letter_not_lower_i letter* | "pi" letter+

rule token = parse
  | whitespace                              { token lexbuf }
  | int as n                                { INT (int_of_string n) }
  | single_letter_variable_name as varname  { VAR (String.of_char varname) }
  | multi_letter_variable_name as varname   { VAR varname }
  | 'e'                                     { E }
  | "pi"                                    { PI }
  | '_'                                     { HOLE }
  | '+'                                     { PLUS }
  | '-'                                     { MINUS }
  | '*'                                     { TIMES }
  | '/'                                     { DIV }
  | '^'                                     { POW }
  | "ln"                                    { LN }
  | '('                                     { LPAREN }
  | ')'                                     { RPAREN }
  | eof                                     { EOF }
  | _ as c                                  { raise (Lexer_error (Printf.sprintf "Unexpected character: %c" c)) }
