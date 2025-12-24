open! Core

type t

val quickterface_math_of_t : t -> Quickterface.Math.t
val literal : string -> t
val pi : t
val e : t
val plus : t
val times : t
val superscript_power : t -> t -> t
val exponential_single_line : t -> t
val ln : t -> t
val concat : t list -> t
val frac : t -> t -> t
val bracketed : t -> t
val partial_derivative : var:t -> t -> t
