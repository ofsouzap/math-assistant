open! Core

type t = Variable of string [@@deriving equal, compare, sexp, show]

module Set = Set.Make (struct
  type t_ = t [@@deriving equal, compare, sexp]
  type t = t_ [@@deriving equal, compare, sexp]

  let compare (Variable v1) (Variable v2) = String.compare v1 v2
end)
