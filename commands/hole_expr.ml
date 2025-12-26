open! Core
open Math_assistant

module Multihole_expr = struct
  type t =
    | Hole
    | Constant of Constant.t
    | Var of Variable.t
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Frac of t * t
    | Pow of { base : t; exponent : t }
    | E_pow of t
    | Ln of t
    | Derivative of { expr : t; var : Variable.t }
  [@@deriving equal, show]
end

module Single_hole_expr = struct
  type t =
    | Hole
    | Add of Expr.t list * t * Expr.t list
    | Sub_left of t * Expr.t
    | Sub_right of Expr.t * t
    | Mul of Expr.t list * t * Expr.t list
    | Frac_numerator of t * Expr.t
    | Frac_denominator of Expr.t * t
    | Pow_base of t * Expr.t
    | Pow_exponent of Expr.t * t
    | E_pow of t
    | Ln of t
    | Derivative of t * Variable.t
  [@@deriving equal, show]

  let rec expr_function_of_t t subexpr =
    match t with
    | Hole -> subexpr
    | Add (lefts, hole, rights) ->
        Expr.Add (lefts @ [ expr_function_of_t hole subexpr ] @ rights)
    | Sub_left (hole, right) -> Sub (expr_function_of_t hole subexpr, right)
    | Sub_right (left, hole) -> Sub (left, expr_function_of_t hole subexpr)
    | Mul (lefts, hole, rights) ->
        Mul (lefts @ [ expr_function_of_t hole subexpr ] @ rights)
    | Frac_numerator (hole, denominator) ->
        Frac (expr_function_of_t hole subexpr, denominator)
    | Frac_denominator (numerator, hole) ->
        Frac (numerator, expr_function_of_t hole subexpr)
    | Pow_base (hole, exponent) ->
        Pow { base = expr_function_of_t hole subexpr; exponent }
    | Pow_exponent (base, hole) ->
        Pow { base; exponent = expr_function_of_t hole subexpr }
    | E_pow hole -> E_pow (expr_function_of_t hole subexpr)
    | Ln hole -> Ln (expr_function_of_t hole subexpr)
    | Derivative (hole, var) ->
        Derivative { expr = expr_function_of_t hole subexpr; var }
end

module Single_hole_expr_of_multihole_expr_converter = struct
  module Single_hole_expr_not_flat = struct
    type t =
      | Hole
      | Add of Expr.t list * t * Expr.t list
      | Sub_left of t * Expr.t
      | Sub_right of Expr.t * t
      | Mul of Expr.t list * t * Expr.t list
      | Frac_numerator of t * Expr.t
      | Frac_denominator of Expr.t * t
      | Pow_base of t * Expr.t
      | Pow_exponent of Expr.t * t
      | E_pow of t
      | Ln of t
      | Derivative of t * Variable.t
  end

  exception No_hole_found
  exception Multiple_holes_found

  type acc = With_hole of Single_hole_expr_not_flat.t | No_hole of Expr.t

  let single_hole_expr_not_flat_of_multihole_expr =
    let rec aux =
      let recurse = aux in
      let do_unop e1 ~no_hole ~with_hole =
        match recurse e1 with
        | No_hole e1' -> No_hole (no_hole e1')
        | With_hole e1' -> With_hole (with_hole e1')
      in
      let do_binop e1 e2 ~no_hole ~right ~left =
        match (recurse e1, recurse e2) with
        | No_hole e1', No_hole e2' -> No_hole (no_hole e1' e2')
        | No_hole e1', With_hole e2' -> With_hole (right e1' e2')
        | With_hole e1', No_hole e2' -> With_hole (left e1' e2')
        | With_hole _, With_hole _ -> raise Multiple_holes_found
      in
      function
      | Multihole_expr.Hole -> With_hole Hole
      | Constant c -> No_hole (Constant c)
      | Var var -> No_hole (Var var)
      | Add (e1, e2) ->
          do_binop e1 e2
            ~no_hole:(fun a b -> Add [ a; b ])
            ~right:(fun a b -> Add ([ a ], b, []))
            ~left:(fun a b -> Add ([], a, [ b ]))
      | Sub (e1, e2) ->
          do_binop e1 e2
            ~no_hole:(fun a b -> Sub (a, b))
            ~right:(fun a b -> Sub_right (a, b))
            ~left:(fun a b -> Sub_left (a, b))
      | Mul (e1, e2) ->
          do_binop e1 e2
            ~no_hole:(fun a b -> Mul [ a; b ])
            ~right:(fun a b -> Mul ([ a ], b, []))
            ~left:(fun a b -> Mul ([], a, [ b ]))
      | Frac (e1, e2) ->
          do_binop e1 e2
            ~no_hole:(fun a b -> Frac (a, b))
            ~right:(fun a b -> Frac_denominator (a, b))
            ~left:(fun a b -> Frac_numerator (a, b))
      | Pow { base; exponent } ->
          do_binop base exponent
            ~no_hole:(fun a b -> Pow { base = a; exponent = b })
            ~right:(fun a b -> Pow_exponent (a, b))
            ~left:(fun a b -> Pow_base (a, b))
      | E_pow e ->
          do_unop e
            ~no_hole:(fun a -> Expr.E_pow a)
            ~with_hole:(fun a -> E_pow a)
      | Ln e ->
          do_unop e ~no_hole:(fun a -> Expr.Ln a) ~with_hole:(fun a -> Ln a)
      | Derivative { expr; var } ->
          do_unop expr
            ~no_hole:(fun a -> Expr.Derivative { expr = a; var })
            ~with_hole:(fun a -> Derivative (a, var))
    in
    fun multihole_expr ->
      match aux multihole_expr with
      | No_hole _ -> raise No_hole_found
      | With_hole single_hole_expr_not_flat -> single_hole_expr_not_flat

  let rec single_hole_expr_of_single_hole_expr_not_flat =
    let recurse = single_hole_expr_of_single_hole_expr_not_flat in
    let rec collect_specific_subterms expr ~find_inside_expr =
      match find_inside_expr expr with
      | Some subexprs ->
          List.concat_map
            ~f:(collect_specific_subterms ~find_inside_expr)
            subexprs
      | None -> [ expr ]
    in
    let rec collect_specific_subterms_with_hole expr ~find_inside_expr
        ~find_inside_with_hole =
      match find_inside_with_hole expr with
      | Some (lefts, hole, rights) ->
          let lefts' =
            List.concat_map
              ~f:(collect_specific_subterms ~find_inside_expr)
              lefts
          in
          let rights' =
            List.concat_map
              ~f:(collect_specific_subterms ~find_inside_expr)
              rights
          in
          let hole_lefts, hole_hole, hole_rights =
            collect_specific_subterms_with_hole ~find_inside_expr
              ~find_inside_with_hole hole
          in
          (lefts' @ hole_lefts, hole_hole, hole_rights @ rights')
      | None -> ([], expr, [])
    in
    let do_collecting_case ~lefts ~hole ~rights ~find_inside_expr
        ~find_inside_with_hole =
      let collector1 = collect_specific_subterms ~find_inside_expr in
      let collector2 =
        collect_specific_subterms_with_hole ~find_inside_expr
          ~find_inside_with_hole
      in
      let lefts' = List.concat_map ~f:collector1 lefts in
      let hole_lefts, hole_hole, hole_rights = collector2 hole in
      let rights' = List.concat_map ~f:collector1 rights in
      (lefts' @ hole_lefts, recurse hole_hole, hole_rights @ rights')
    in
    function
    | Single_hole_expr_not_flat.Hole -> Single_hole_expr.Hole
    | Add (lefts, hole, rights) ->
        let lefts', hole', rights' =
          do_collecting_case ~lefts ~hole ~rights
            ~find_inside_expr:(function
              | Expr.Add args -> Some args | _ -> None)
            ~find_inside_with_hole:(function
              | Single_hole_expr_not_flat.Add (lefts, hole, rights) ->
                  Some (lefts, hole, rights)
              | _ -> None)
        in
        Add (lefts', hole', rights')
    | Sub_left (e1, e2) -> Sub_left (recurse e1, e2)
    | Sub_right (e1, e2) -> Sub_right (e1, recurse e2)
    | Mul (lefts, hole, rights) ->
        let lefts', hole', rights' =
          do_collecting_case ~lefts ~hole ~rights
            ~find_inside_expr:(function
              | Expr.Mul args -> Some args | _ -> None)
            ~find_inside_with_hole:(function
              | Single_hole_expr_not_flat.Mul (lefts, hole, rights) ->
                  Some (lefts, hole, rights)
              | _ -> None)
        in
        Mul (lefts', hole', rights')
    | Frac_numerator (e1, e2) -> Frac_numerator (recurse e1, e2)
    | Frac_denominator (e1, e2) -> Frac_denominator (e1, recurse e2)
    | Pow_base (e1, e2) -> Pow_base (recurse e1, e2)
    | Pow_exponent (e1, e2) -> Pow_exponent (e1, recurse e2)
    | E_pow e1 -> E_pow (recurse e1)
    | Ln e1 -> Ln (recurse e1)
    | Derivative (e1, var) -> Derivative (recurse e1, var)

  let single_hole_expr_of_multihole_expr =
    Fn.compose single_hole_expr_of_single_hole_expr_not_flat
      single_hole_expr_not_flat_of_multihole_expr
end
