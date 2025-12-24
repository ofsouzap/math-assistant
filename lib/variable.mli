open! Core

type t [@@deriving equal, sexp, show]

val latex_of_t : t -> Latex_builder.t
val of_string : string -> (t, Sexp.t) Result.t
val of_string_exn : string -> t

module Set : Set.S with type Elt.t = t
