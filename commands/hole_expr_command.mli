open! Core
open Math_assistant

type t

val expr_function_of_t : t -> Expr.t -> Expr.t

module Parser : sig
  module Error : sig
    type t =
      | No_matching_command of string
      | Variable_name_error of Sexp.t
      | Cannot_parse_constant_from of string
      | Lexer_error of string
      | No_hole_found
      | Multiple_holes_found
    [@@deriving sexp, equal, show]

    val cannot_parse_constant_from : string -> t
    val no_matching_command : string -> t

    module For_testing : sig
      val testable : t Alcotest.testable
    end
  end

  include
    Command_parser_utils.Parser.S
      with type output := t
       and module Error := Error
end

module For_testing : sig
  val of_hole_expr : Hole_expr.Single_hole_expr.t -> t
  val testable : t Alcotest.testable
end
