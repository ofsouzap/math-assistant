open! Core
include Expr_manipulations_intf

module Manipulation = struct
  module type S = Manipulation.S
end

module Reduce_constants = struct
  type t = unit

  let make () = ()

  module Accumulator = struct
    type t = {
      int_lit_res : int;
      pi_count : int;
      e_count : int;
      rest_rev : Expr.t list;
    }

    let init = { int_lit_res = 0; pi_count = 0; e_count = 0; rest_rev = [] }

    let accumulate_constant ~fold_int_lit t = function
      | Constant.Int_lit n ->
          { t with int_lit_res = fold_int_lit t.int_lit_res n }
      | Pi -> { t with pi_count = t.pi_count + 1 }
      | E -> { t with e_count = t.e_count + 1 }

    let accumulate_constants_in_expressions ~fold_int_lit : Expr.t list -> t =
      List.fold ~init ~f:(fun t -> function
        | Expr.Constant c -> accumulate_constant ~fold_int_lit t c
        | expr -> { t with rest_rev = expr :: t.rest_rev })
  end

  let rec apply t = function
    | Expr.Constant c -> Expr.Constant c
    | Var v -> Var v
    | Add args ->
        let { Accumulator.int_lit_res; pi_count; e_count; rest_rev } =
          Accumulator.accumulate_constants_in_expressions ~fold_int_lit:( + )
            args
        in
        Add
          (Constant (Int_lit int_lit_res)
          :: Mul [ Constant (Int_lit pi_count); Constant Pi ]
          :: Mul [ Constant (Int_lit e_count); Constant E ]
          :: List.rev rest_rev)
    | Mul args ->
        let { Accumulator.int_lit_res; pi_count; e_count; rest_rev } =
          Accumulator.accumulate_constants_in_expressions ~fold_int_lit:( * )
            args
        in
        Mul
          (Constant (Int_lit int_lit_res)
          :: Pow { base = Constant Pi; exponent = Constant (Int_lit pi_count) }
          :: Pow { base = Constant E; exponent = Constant (Int_lit e_count) }
          :: List.rev rest_rev)
    | Pow { base; exponent } ->
        Pow { base = apply t base; exponent = apply t exponent }
    | E_pow exponent -> E_pow (apply t exponent)
    | Frac (num, denom) -> Frac (apply t num, apply t denom)
end
