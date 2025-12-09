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

let rec latex_of_t =
  (* TODO - determine when brackets are needed and insert them *)
  let open Latex_builder in
  let latex_of_monoid ?wrapper subterm_latexs ~op =
    let wrapper_fn = Option.value wrapper ~default:Fn.id in
    wrapper_fn (concat (List.intersperse ~sep:(literal op) subterm_latexs))
  in
  function
  | Constant c -> Constant.latex_of_t c
  | Var (Variable varname) -> literal varname
  | Add args ->
      let term_latexs = List.map ~f:latex_of_t args in
      latex_of_monoid ~op:"+" term_latexs
  | Sub (e_pos, e_neg) ->
      let pos_latex = latex_of_t e_pos in
      let neg_latex = latex_of_t e_neg in
      concat [ pos_latex; literal " - "; neg_latex ]
  | Mul args ->
      let term_latexs = List.map ~f:latex_of_t args in
      latex_of_monoid ~wrapper:parens ~op:"\\cdot" term_latexs
  | Frac (num, denom) -> command "frac" [ latex_of_t num; latex_of_t denom ]
  | Pow { base; exponent } ->
      let base_latex = latex_of_t base in
      let exponent_latex = latex_of_t exponent in
      concat [ base_latex; Latex_builder.literal "^"; exponent_latex ]
  | E_pow exponent ->
      let exponent_depth = structure_depth exponent in
      let exponent_latex = latex_of_t exponent in
      if exponent_depth <= 2 (* This constant is chosen manually *) then
        concat [ literal "e"; literal "^"; exponent_latex ]
      else concat [ command "exp" []; square_brackets exponent_latex ]
  | Ln arg -> command "ln" [ latex_of_t arg ]
  | Derivative { expr; var = Variable varname } ->
      concat
        [
          command "frac"
            [ literal "\\del"; concat [ literal "\\del"; literal varname ] ];
          latex_of_t expr;
        ]

module For_testing = struct
  let testable = Alcotest.testable pp equal
end
