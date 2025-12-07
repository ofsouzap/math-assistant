open! Core

type t =
  | Constant of Constant.t
  | Var of string
  | Add of t list
  | Mul of t list
  | Pow of { base : t; exponent : t }
  | E_pow of t
  | Frac of t * t
[@@deriving equal]

let rec structure_depth = function
  | Constant _ | Var _ -> 0
  | E_pow exponent -> 1 + structure_depth exponent
  | Pow { base = a; exponent = b } | Frac (a, b) ->
      let base_depth = structure_depth a in
      let exponent_depth = structure_depth b in
      1 + Int.max base_depth exponent_depth
  | Add args | Mul args ->
      List.map ~f:structure_depth args
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
      |> fun d -> d + 1

let rec latex_of_t =
  (* TODO - determine when brackets are needed and insert them *)
  let latex_of_monoid ?wrapper subterm_latexs ~op =
    let wrapper_fn = Option.value wrapper ~default:Fn.id in
    wrapper_fn
      (Latex_builder.concat
         (List.intersperse ~sep:(Latex_builder.literal op) subterm_latexs))
  in
  function
  | Constant c -> Constant.latex_of_t c
  | Var varname -> Latex_builder.literal varname
  | Add args ->
      let term_latexs = List.map ~f:latex_of_t args in
      latex_of_monoid ~op:"+" term_latexs
  | Mul args ->
      let term_latexs = List.map ~f:latex_of_t args in
      latex_of_monoid ~wrapper:Latex_builder.parens ~op:"\\cdot" term_latexs
  | Pow { base; exponent } ->
      let base_latex = latex_of_t base in
      let exponent_latex = latex_of_t exponent in
      Latex_builder.concat
        [ base_latex; Latex_builder.literal "^"; exponent_latex ]
  | E_pow exponent ->
      let exponent_depth = structure_depth exponent in
      let exponent_latex = latex_of_t exponent in
      if exponent_depth <= 2 (* This constant is chosen manually *) then
        Latex_builder.concat
          [
            Latex_builder.literal "e"; Latex_builder.literal "^"; exponent_latex;
          ]
      else
        Latex_builder.concat
          [
            Latex_builder.command "exp" [];
            Latex_builder.square_brackets exponent_latex;
          ]
  | Frac (num, denom) ->
      Latex_builder.command "frac" [ latex_of_t num; latex_of_t denom ]
