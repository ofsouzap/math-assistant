open! Core

module type S = sig
  type t

  val neutral : t
  val op : t -> t -> t
end
