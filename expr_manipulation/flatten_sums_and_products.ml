open! Core
open! Math_assistant

type t = unit

let make () = ()

let apply =
  let rec flatten = function
    | Expr.Constant c -> Expr.Constant c
    | Var v -> Var v
    | Add args ->
        let processed_args =
          List.fold args ~init:[] ~f:(fun acc_rev arg ->
              match flatten arg with
              | Expr.Add add_args -> List.rev add_args @ acc_rev
              | flattened_arg -> flattened_arg :: acc_rev)
          |> List.rev
        in
        Add processed_args
    | Sub (e_pos, e_neg) ->
        let flat_pos = flatten e_pos in
        let flat_neg = flatten e_neg in
        Sub (flat_pos, flat_neg)
    | Mul args ->
        let processed_args =
          List.fold args ~init:[] ~f:(fun acc_rev arg ->
              match flatten arg with
              | Expr.Mul mul_args -> List.rev mul_args @ acc_rev
              | flattened_arg -> flattened_arg :: acc_rev)
          |> List.rev
        in
        Mul processed_args
    | Frac (num, denom) -> Frac (flatten num, flatten denom)
    | Pow { base; exponent } ->
        Pow { base = flatten base; exponent = flatten exponent }
    | E_pow exponent -> E_pow (flatten exponent)
    | Ln arg -> Ln (flatten arg)
    | Derivative { expr; var } -> Derivative { expr = flatten expr; var }
  in
  fun () -> flatten
