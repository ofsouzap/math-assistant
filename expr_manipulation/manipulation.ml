open! Core
open! Math_assistant

(** Note that this general signature won't give any nice guarantees about the
    result of the manipulations. However this would be a pain so I won't do it.

    For example, the integer literal reduction manipulation could be written to
    have an output type that only allows integer literals in at most one
    position of a sum or a product, however this would take a huge amount of
    implementation just for that one manipulation and isn't, therefore, worth
    it. *)
module type S = sig
  type make_args
  type t

  val make : make_args -> t
  val apply : t -> Expr.t -> Expr.t
end
