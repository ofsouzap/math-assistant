module type S = sig
  type t

  val monoid_neutral : t
  val monoid_op : t -> t -> t
end

module Int_addition : S with type t = int
module Int_multiplication : S with type t = int
