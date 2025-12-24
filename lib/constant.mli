open! Core

type t = Int_lit of int | Pi | E [@@deriving equal, show]

val zero : t
val one : t
val pi : t
val e : t
val quickterface_math_of_t : t -> Quickterface_math_builder.t

module For_testing : sig
  val testable : t Alcotest.testable
end
