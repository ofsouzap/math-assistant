open! Core

type t [@@deriving equal, sexp, show]

val of_string : string -> (t, Sexp.t) Result.t
val of_string_exn : string -> t
val quickterface_math_of_t : t -> Quickterface_math_builder.t

module Set : Set.S with type Elt.t = t
