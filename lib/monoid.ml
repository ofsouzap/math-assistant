open! Core

module type S = sig
  type t

  val monoid_neutral : t
  val monoid_op : t -> t -> t
end

module Int_addition = struct
  type t = int

  let monoid_neutral = 0
  let monoid_op = ( + )
end

module Int_multiplication = struct
  type t = int

  let monoid_neutral = 1
  let monoid_op = ( * )
end
