open! Core

module Multihole_expr : sig
  type t =
    | Hole
    | Constant of Math_assistant.Constant.t
    | Var of Math_assistant.Variable.t
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Frac of t * t
    | Pow of { base : t; exponent : t }
    | E_pow of t
    | Ln of t
    | Derivative of { expr : t; var : Math_assistant.Variable.t }
  [@@deriving equal, show]
end

module Single_hole_expr : sig
  type t =
    | Hole
    | Add of Math_assistant.Expr.t list * t * Math_assistant.Expr.t list
    | Sub_left of t * Math_assistant.Expr.t
    | Sub_right of Math_assistant.Expr.t * t
    | Mul of Math_assistant.Expr.t list * t * Math_assistant.Expr.t list
    | Frac_numerator of t * Math_assistant.Expr.t
    | Frac_denominator of Math_assistant.Expr.t * t
    | Pow_base of t * Math_assistant.Expr.t
    | Pow_exponent of Math_assistant.Expr.t * t
    | E_pow of t
    | Ln of t
    | Derivative of t * Math_assistant.Variable.t
  [@@deriving equal, show]

  val expr_function_of_t : t -> Math_assistant.Expr.t -> Math_assistant.Expr.t
end

module Single_hole_expr_of_multihole_expr_converter : sig
  exception No_hole_found
  exception Multiple_holes_found

  val single_hole_expr_of_multihole_expr :
    Multihole_expr.t -> Single_hole_expr.t
end
