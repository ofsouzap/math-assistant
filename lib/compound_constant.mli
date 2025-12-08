(** Provides manipulation of compound collections of constants, that have a
    value for each of the constant constructors.

    There isn't a fixed interpretation of these records so be careful which
    functions are used on them *)

open! Core

module type Base_signature = sig
  type t [@@deriving show]
end

(** Basic implementation, without any pre-defined interpretation *)
module Base : sig
  type t = { int_lit : int; pi : int; e : int } [@@deriving equal, show]

  include Base_signature with type t := t

  val empty : t
end

(** Considering the [int_lit] field as some integer, monoidal accumulator, and
    the [pi] and [e] fields as incrementing counts. *)
module Int_lit_monoid_pi_e_counts : sig
  module type S = sig
    include Base_signature

    val append : t -> t -> t
    val append_constant : t -> Constant.t -> t
    val t_of_base : Base.t -> t
    val t_of_constant : Constant.t -> t

    module Appended_from_expressions : sig
      type nonrec t = { value : t; rest_rev : Expr.t list }
    end

    val append_constants_in_expressions :
      Expr.t list -> Appended_from_expressions.t

    module Monoid : Monoid.S with type t = t
  end

  module Make (_ : Monoid.S with type t = int) : S
end

(** Interpreting [{ int_lit; pi = pi_count; e = e_count }] as the numerical
    value [int_lit + pi_count*pi + e_count*e].

    This means we can consider the int lit as a sum accumulator, and the pi and
    e fields as counts *)
module Summed_constant : sig
  type t [@@deriving equal, show]

  include Int_lit_monoid_pi_e_counts.S with type t := t

  val zero : t
  val add : t -> t -> t
  val add_constant : t -> Constant.t -> t
  val negate : t -> t
  val sub : t -> t -> t
  val sub_constant : t -> Constant.t -> t
  val sum_exprs_of_t : ?tail_expressions:Expr.t list -> t -> Expr.t list
  val expr_of_t : ?tail_expressions:Expr.t list -> t -> Expr.t
end

(** Interpreting [{ int_lit; pi = pi_count; e = e_count }] as the numerical
    value [int_lit * pi^{pi_count} + e^{e_count}].

    This means we can consider the int lit as a product accumulator, and the pi
    and e fields as counts *)
module Multiplied_constant : sig
  include Int_lit_monoid_pi_e_counts.S with type t = Base.t

  val one : t
  val pi : t
  val e : t
  val multiply : t -> t -> t
  val mul_constant : t -> Constant.t -> t
  val mul_exprs_of_t : ?tail_expressions:Expr.t list -> t -> Expr.t list
  val expr_of_t : ?tail_expressions:Expr.t list -> t -> Expr.t
end
