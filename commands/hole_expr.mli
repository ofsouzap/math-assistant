open! Core
open Math_assistant

type t

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
