open! Core
open Math_assistant

type t

val expr_function_of_t : t -> Expr.t -> Expr.t

module Parser : Command_parser_utils.Parser.S with type output := t

module For_testing : sig
  val add : Constant.t -> t
  val times : Constant.t -> t
  val divide_by : Constant.t -> t
  val apply_derivatives : t
  val evaluate_constant_expressions : t
  val take_derivative : Variable.t -> t
  val testable : t Alcotest.testable
end
