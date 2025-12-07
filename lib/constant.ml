open! Core

type t = Int_lit of int | Pi | E [@@deriving equal]

let latex_of_t t =
  match t with
  | Int_lit n -> string_of_int n |> Latex_builder.literal
  | Pi -> Latex_builder.pi
  | E -> Latex_builder.e
