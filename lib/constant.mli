open! Core

type t = Int_lit of int | Pi | E [@@deriving equal]

val latex_of_t : t -> Latex_builder.t
