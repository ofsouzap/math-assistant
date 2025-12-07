open! Core

type t = Int_lit of int | Pi | E [@@deriving equal, show]

val latex_of_t : t -> Latex_builder.t

module For_testing : sig
  val testable : t Alcotest.testable
end
