open! Core
open! Math_assistant

type t = unit

let make () = ()

let rec apply t = function
  | Expr.Constant c -> Expr.Constant c
  | Var v -> Var v
  | Add args ->
      let%tydi { value; rest_rev } =
        Compound_constant.Summed_constant.append_constants_in_expressions args
      in
      Compound_constant.Summed_constant.expr_of_t
        ~tail_expressions:(List.rev rest_rev) value
  | Sub (e_pos, e_neg) -> (
      (* Recurse *)
      let e_pos' = apply t e_pos in
      let e_neg' = apply t e_neg in
      (* Try simplify *)
      match (e_pos', e_neg') with
      | Expr.Constant (Int_lit n_pos), Expr.Constant (Int_lit n_neg) ->
          Expr.Constant (Int_lit (n_pos - n_neg))
      | Constant Pi, Constant Pi | Constant E, Constant E ->
          (* pi - pi = e - e = 0 *)
          Constant (Int_lit 0)
      | ( Mul [ Constant (Int_lit n_pos); Constant Pi ],
          Mul [ Constant (Int_lit n_neg); Constant Pi ] ) ->
          (* n1 * pi - n2 * pi = (n1 - n2) * pi *)
          Mul [ Constant (Int_lit (n_pos - n_neg)); Constant Pi ]
      | ( Mul [ Constant (Int_lit n_pos); Constant E ],
          Mul [ Constant (Int_lit n_neg); Constant E ] ) ->
          (* n1 * e - n2 * e = (n1 - n2) * e *)
          Mul [ Constant (Int_lit (n_pos - n_neg)); Constant E ]
      | _ -> Sub (e_pos', e_neg'))
  | Mul args ->
      let%tydi { value; rest_rev } =
        Compound_constant.Multiplied_constant.append_constants_in_expressions
          args
      in
      Compound_constant.Multiplied_constant.expr_of_t
        ~tail_expressions:(List.rev rest_rev) value
  | Frac (num, denom) -> Frac (apply t num, apply t denom)
  | Pow { base; exponent } ->
      Pow { base = apply t base; exponent = apply t exponent }
  | E_pow exponent -> E_pow (apply t exponent)
  | Ln arg -> Ln (apply t arg)
  | Derivative { expr; var } -> Derivative { expr = apply t expr; var }
