open! Core

module Manipulation = struct
  (** Note that this general signature won't give any nice guarantees about the
      result of the manipulations. However this would be a pain so I won't do
      it.contents.

      For example, the integer literal reduction manipulation could be written
      to have an output type that only allows integer literals in at most one
      position of a sum or a product, however this would take a huge amount of
      implementation just for that one manipulation and isn't, therefore, worth
      it. *)
  module type S = sig
    type make_args
    type t

    val make : make_args -> t
    val apply : t -> Expr.t -> Expr.t
  end
end

module type S = sig
  module Manipulation : module type of Manipulation
  module Flatten_sums_and_products : Manipulation.S with type make_args := unit

  (* module Distribute_products_over_sums : Manipulation.S with type make_args := unit *)
  module Reduce_constants : Manipulation.S with type make_args := unit

  (* module Reduce_direct_fractions : Manipulation.S with type make_args := unit *)
  (* module Substitute_variable :
    Manipulation.S with type make_args := string * Expr.t *)

  module Take_derivative : Manipulation.S with type make_args := Variable.t
end
