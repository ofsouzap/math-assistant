open! Core
open Math_assistant

type t =
  | Add of Constant.t
  | Times of Constant.t
  | Divide_by of Constant.t
  | Apply_derivatives
  | Evaluate_constant_expressions
  | Take_derivative of Variable.t

val expr_function_of_t : t -> Expr.t -> Expr.t

module Parser : sig
  type hole_expr := t
  type t

  module Error : sig
    type t [@@deriving sexp, show]
  end

  val make : unit -> t
  val parse : t -> string -> (hole_expr, Error.t) Result.t
end
