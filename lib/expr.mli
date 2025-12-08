open! Core

type t =
  | Constant of Constant.t
  | Var of Variable.t
  | Add of t list
  | Sub of t * t
  | Mul of t list
  | Frac of t * t
  | Pow of { base : t; exponent : t }
  | E_pow of t
  (* | Log of {base: t; arg: t} *)
  | Ln of t
  | Derivative of { expr : t; var : Variable.t }
    (* | Indefinite_integral of {expr : t; var : string} *)
[@@deriving equal, show]

val latex_of_t : t -> Latex_builder.t

module For_testing : sig
  val testable : t Alcotest.testable
end
