open! Core
open Math_assistant

type t

val expr_function_of_t : t -> Expr.t -> Expr.t

module Parser : Command_parser_utils.Parser.S with type output := t
