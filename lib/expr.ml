open! Core

type t =
  | Constant of Constant.t
  | Var of Variable.t
  | Add of t list
  | Sub of t * t
  | Mul of t list
  | Frac of t * t
  | Pow of { base : t; exponent : t }
  | E_pow of t
  | Ln of t
  | Derivative of { expr : t; var : Variable.t }
[@@deriving equal, show]

let rec structure_depth = function
  | Constant _ | Var _ -> 0
  | E_pow e1 | Ln e1 | Derivative { expr = e1; var = _ } ->
      1 + structure_depth e1
  | Sub (e1, e2) | Frac (e1, e2) | Pow { base = e1; exponent = e2 } ->
      1 + Int.max (structure_depth e1) (structure_depth e2)
  | Add args | Mul args ->
      List.map ~f:structure_depth args
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
      |> fun d -> d + 1

let rec free_variables = function
  | Constant _ -> Variable.Set.empty
  | Var v -> Variable.Set.singleton v
  | E_pow e | Ln e -> free_variables e
  | Sub (e1, e2) | Frac (e1, e2) | Pow { base = e1; exponent = e2 } ->
      Set.union (free_variables e1) (free_variables e2)
  | Add args | Mul args ->
      List.fold args ~init:Variable.Set.empty ~f:(fun acc expr ->
          Set.union acc (free_variables expr))
  | Derivative { expr; var = _ } ->
      (* We cannot know if the variable being differentiated with respect to is free or bound in the derivative *)
      free_variables expr

let rec quickterface_math_of_t =
  (* TODO - determine when brackets are needed and insert them *)
  let _hold_quickterface_math_of_t = quickterface_math_of_t in
  let open Quickterface_math_builder in
  let quickterface_math_of_t = _hold_quickterface_math_of_t in

  let of_monoid ?wrapper subterms ~op =
    let wrapper_fn = Option.value wrapper ~default:Fn.id in
    wrapper_fn (concat (List.intersperse ~sep:op subterms))
  in
  function
  | Constant c -> Constant.quickterface_math_of_t c
  | Var var -> Variable.quickterface_math_of_t var
  | Add args ->
      let term_latexs = List.map ~f:quickterface_math_of_t args in
      of_monoid ~op:plus term_latexs
  | Sub (e_pos, e_neg) ->
      let pos_latex = quickterface_math_of_t e_pos in
      let neg_latex = quickterface_math_of_t e_neg in
      concat [ pos_latex; literal " - "; neg_latex ]
  | Mul args ->
      let term_latexs = List.map ~f:quickterface_math_of_t args in
      of_monoid ~wrapper:bracketed ~op:times term_latexs
  | Frac (num, denom) ->
      frac (quickterface_math_of_t num) (quickterface_math_of_t denom)
  | Pow { base; exponent } ->
      let base_math = quickterface_math_of_t base in
      let exponent_math = quickterface_math_of_t exponent in
      superscript_power base_math exponent_math
  | E_pow exponent ->
      let exponent_depth = structure_depth exponent in
      let exponent_math = quickterface_math_of_t exponent in
      if exponent_depth <= 2 (* This constant is chosen manually *) then
        superscript_power e exponent_math
      else exponential_single_line exponent_math
  | Ln arg -> ln (quickterface_math_of_t arg)
  | Derivative { expr; var } ->
      partial_derivative
        ~var:(Variable.quickterface_math_of_t var)
        (quickterface_math_of_t expr)

module For_testing = struct
  let testable = Alcotest.testable pp equal
end
