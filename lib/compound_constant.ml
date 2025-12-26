open! Core

module type Base_signature = sig
  type t [@@deriving show]
end

module Base = struct
  type t = { int_lit : int; pi : int; e : int } [@@deriving equal, show]

  let empty = { int_lit = 0; pi = 0; e = 0 }
end

module Int_lit_monoid_pi_e_counts = struct
  module type S = sig
    include Base_signature

    val append : t -> t -> t
    val append_constant : t -> Constant.t -> t
    val t_of_base : Base.t -> t
    val t_of_constant : Constant.t -> t

    module Appended_from_expressions : sig
      type nonrec t = { value : t; rest_rev : Expr.t list }
    end

    val append_constants_in_expressions :
      Expr.t list -> Appended_from_expressions.t

    module Monoid : Monoid.S with type t = t
  end

  module Make (Int_monoid : Monoid.S with type t = int) = struct
    type t = Base.t = { int_lit : int; pi : int; e : int }
    [@@deriving equal, show]

    let empty = { int_lit = Int_monoid.monoid_neutral; pi = 0; e = 0 }

    let append { int_lit = int_lit_x; pi = pi_x; e = e_x }
        { int_lit = int_lit_y; pi = pi_y; e = e_y } =
      {
        int_lit = Int_monoid.monoid_op int_lit_x int_lit_y;
        pi = pi_x + pi_y;
        e = e_x + e_y;
      }

    let append_constant t = function
      | Constant.Int_lit n ->
          { t with int_lit = Int_monoid.monoid_op t.int_lit n }
      | Pi -> { t with pi = t.pi + 1 }
      | E -> { t with e = t.e + 1 }

    let t_of_constant = append_constant empty
    let t_of_base = Fn.id

    module Appended_from_expressions = struct
      type nonrec t = { value : t; rest_rev : Expr.t list }

      let empty =
        {
          value = { int_lit = Int_monoid.monoid_neutral; pi = 0; e = 0 };
          rest_rev = [];
        }

      let append_constant acc c =
        { acc with value = append_constant acc.value c }
    end

    let append_constants_in_expressions =
      List.fold ~init:Appended_from_expressions.empty ~f:(fun acc -> function
        | Expr.Constant c -> Appended_from_expressions.append_constant acc c
        | expr -> { acc with rest_rev = expr :: acc.rest_rev })

    let collect_non_empty_terms_from_counts ~counted_term_constructor
        { Base.int_lit; pi; e } =
      let int_lit_term =
        (if int_lit = Int_monoid.monoid_neutral then None
         else Some (Expr.Constant (Int_lit int_lit)))
        |> Option.to_list
      in
      let pi_term =
        (if pi = 0 then None
         else if pi = 1 then Some (Expr.Constant Pi)
         else
           Some
             (counted_term_constructor ~n:(Expr.Constant (Int_lit pi))
                ~value:(Expr.Constant Pi)))
        |> Option.to_list
      in
      let e_term =
        (if e = 0 then None
         else if e = 1 then Some (Expr.Constant E)
         else
           Some
             (counted_term_constructor ~n:(Expr.Constant (Int_lit e))
                ~value:(Expr.Constant E)))
        |> Option.to_list
      in
      int_lit_term @ pi_term @ e_term

    module Monoid = struct
      type nonrec t = t

      let monoid_neutral = empty
      let monoid_op = append
    end
  end
end

module Summed_constant = struct
  include Int_lit_monoid_pi_e_counts.Make (Monoid.Int_addition)

  let zero = empty
  let add = append
  let add_constant = append_constant
  let negate { int_lit; pi; e } = { int_lit = -int_lit; pi = -pi; e = -e }
  let sub t1 t2 = add t1 (negate t2)
  let sub_constant t c = sub t (t_of_constant c)

  let sum_exprs_of_t ?(tail_expressions = []) t =
    let terms =
      collect_non_empty_terms_from_counts
        ~counted_term_constructor:(fun ~n ~value -> Expr.Mul [ n; value ])
        t
      @ tail_expressions
    in
    terms

  let expr_of_t ?tail_expressions t =
    let terms = sum_exprs_of_t ?tail_expressions t in
    match terms with
    | [] -> Expr.Constant (Constant.Int_lit 0)
    | [ single ] -> single
    | terms -> Expr.Add terms
end

module Multiplied_constant = struct
  include Int_lit_monoid_pi_e_counts.Make (Monoid.Int_multiplication)

  let one = empty
  let pi = append_constant one Pi
  let e = append_constant one E
  let multiply = append
  let mul_constant = append_constant

  let mul_exprs_of_t ?(tail_expressions = []) t =
    let terms =
      collect_non_empty_terms_from_counts
        ~counted_term_constructor:(fun ~n ~value ->
          Expr.Pow { base = value; exponent = n })
        t
      @ tail_expressions
    in
    terms

  let expr_of_t ?tail_expressions t =
    let terms = mul_exprs_of_t ?tail_expressions t in
    match terms with
    | [] -> Expr.Constant (Constant.Int_lit 1)
    | [ single ] -> single
    | terms -> Expr.Mul terms
end
