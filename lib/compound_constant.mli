(** Provides manipulation of compound collections of constants, that have a
    value for each of the constant constructors.

    There isn't a fixed interpretation of these records so be careful which
    functions are used on them *)

open! Core

module Base : sig
  type t = { int_lit : int; pi : int; e : int } [@@deriving equal, show, fields]

  val empty : t
  val t_of_constant : Constant.t -> t
end

(** Considering the [int_lit] field as some integer, monoidal accumulator, and
    the [pi] and [e] fields as incrementing counts.

    The monoid module parameter is for handling the integer literal accumulator
*)
module Int_lit_acc_pi_e_counts : sig
  module type S = sig
    type t [@@deriving show]

    val t_of_base : Base.t -> t
    val accumulate_constant : t -> Constant.t -> t

    module Accumulated_from_expressions : sig
      type nonrec t = { value : t; rest_rev : Expr.t list }
    end

    val accumulate_constants_in_expressions :
      Expr.t list -> Accumulated_from_expressions.t
  end

  module Make (_ : Monoid.S with type t = int) : S
end

(** Considering the int lit as a sum accumulator, and the pi and e fields as
    counts *)
module Int_lit_sum_pi_e_counts : sig
  include Int_lit_acc_pi_e_counts.S with type t = Base.t

  val expr_of_t : ?tail_expressions:Expr.t list -> t -> Expr.t
end

(** Considering the int lit as a product accumulator, and the pi and e fields as
    counts *)
module Int_lit_product_pi_e_counts : sig
  include Int_lit_acc_pi_e_counts.S with type t = Base.t

  val expr_of_t : ?tail_expressions:Expr.t list -> t -> Expr.t
end
