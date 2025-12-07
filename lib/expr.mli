open! Core

type t =
  | Constant of Constant.t
  | Var of string
  | Add of t list
  | Mul of t list
  | Pow of { base : t; exponent : t }
  | E_pow of t
  | Frac of t * t
(* | Log of {base: t; arg: t} *)
(* | Ln of t *)
(* | Derivative of {expr : t; var : string}
  | Definite_integral of {expr : t; var : string} *)
[@@deriving equal]

val latex_of_t : t -> Latex_builder.t
