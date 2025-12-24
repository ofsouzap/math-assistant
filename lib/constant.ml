open! Core

type t = Int_lit of int | Pi | E [@@deriving equal, show]

let zero = Int_lit 0
let one = Int_lit 1
let pi = Pi
let e = E

let quickterface_math_of_t t =
  match t with
  | Int_lit n -> string_of_int n |> Quickterface_math_builder.literal
  | Pi -> Quickterface_math_builder.pi
  | E -> Quickterface_math_builder.e

module For_testing = struct
  let testable = Alcotest.testable pp equal
end
