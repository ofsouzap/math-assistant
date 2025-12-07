open! Core

type t

val to_string : t -> string
val literal : string -> t
val pi : t
val e : t
val concat : t list -> t
val command : string -> t list -> t
val left_right_wrapper : string -> string -> t -> t
val parens : t -> t
val braces : t -> t
val square_brackets : t -> t
val depth : t -> int
