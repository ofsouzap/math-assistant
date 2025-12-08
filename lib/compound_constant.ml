open! Core

module Base = struct
  type t = { int_lit : int; pi : int; e : int } [@@deriving equal, show, fields]

  let empty = { int_lit = 0; pi = 0; e = 0 }

  let t_of_constant = function
    | Constant.Int_lit n -> { int_lit = n; pi = 0; e = 0 }
    | Pi -> { int_lit = 0; pi = 1; e = 0 }
    | E -> { int_lit = 0; pi = 0; e = 1 }
end

module Int_lit_acc_pi_e_counts = struct
  module type S = sig
    type t [@@deriving show]

    val t_of_base : Base.t -> t
    val accumulate_constant : t -> Constant.t -> t

    module Accumulated_from_expressions : sig
      type nonrec t = { value : t; rest_rev : Expr.t list }
    end

    val accumulate_constants_in_expressions :
      Expr.t list -> Accumulated_from_expressions.t
  end

  module Make (Monoid : Monoid.S with type t = int) = struct
    type t = Base.t = { int_lit : int; pi : int; e : int } [@@deriving show]

    let t_of_base = Fn.id

    let accumulate_constant t = function
      | Constant.Int_lit n -> { t with int_lit = Monoid.op t.int_lit n }
      | Pi -> { t with pi = t.pi + 1 }
      | E -> { t with e = t.e + 1 }

    module Accumulated_from_expressions = struct
      type nonrec t = { value : t; rest_rev : Expr.t list }

      let empty =
        { value = { int_lit = Monoid.neutral; pi = 0; e = 0 }; rest_rev = [] }

      let accumulate_constant acc c =
        { acc with value = accumulate_constant acc.value c }
    end

    let accumulate_constants_in_expressions =
      List.fold ~init:Accumulated_from_expressions.empty ~f:(fun acc -> function
        | Expr.Constant c ->
            Accumulated_from_expressions.accumulate_constant acc c
        | expr -> { acc with rest_rev = expr :: acc.rest_rev })

    let collect_non_empty_terms_from_counts ~counted_term_constructor
        { int_lit; pi; e } =
      let int_lit_term =
        (if int_lit = Monoid.neutral then None
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
  end
end

module Int_lit_sum_pi_e_counts = struct
  include Int_lit_acc_pi_e_counts.Make (struct
    type t = int

    let neutral = 0
    let op = ( + )
  end)

  let expr_of_t ?(tail_expressions = []) t =
    let terms =
      collect_non_empty_terms_from_counts
        ~counted_term_constructor:(fun ~n ~value -> Expr.Mul [ n; value ])
        t
      @ tail_expressions
    in
    match terms with
    | [] -> Expr.Constant (Constant.Int_lit 0)
    | [ single ] -> single
    | terms -> Expr.Add terms
end

module Int_lit_product_pi_e_counts = struct
  include Int_lit_acc_pi_e_counts.Make (struct
    type t = int

    let neutral = 1
    let op = ( * )
  end)

  let expr_of_t ?(tail_expressions = []) t =
    let terms =
      collect_non_empty_terms_from_counts
        ~counted_term_constructor:(fun ~n ~value ->
          Expr.Pow { base = value; exponent = n })
        t
      @ tail_expressions
    in
    match terms with
    | [] -> Expr.Constant (Constant.Int_lit 1)
    | [ single ] -> single
    | terms -> Expr.Mul terms
end
