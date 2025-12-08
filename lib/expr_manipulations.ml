open! Core
include Expr_manipulations_intf

module Manipulation = struct
  module type S = Manipulation.S
end

module Flatten_sums_and_products = struct
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
end

module Reduce_constants = struct
  type t = unit

  let make () = ()

  module Accumulator = struct
    type t = {
      int_lit_neutral : int;
      int_lit_fold : int -> int -> int;
      int_lit_res : int;
      pi_count : int;
      e_count : int;
      rest_rev : Expr.t list;
    }

    let init ~int_lit_neutral ~int_lit_fold =
      {
        int_lit_neutral;
        int_lit_fold;
        int_lit_res = int_lit_neutral;
        pi_count = 0;
        e_count = 0;
        rest_rev = [];
      }

    let accumulate_constant t = function
      | Constant.Int_lit n ->
          { t with int_lit_res = t.int_lit_fold t.int_lit_res n }
      | Pi -> { t with pi_count = t.pi_count + 1 }
      | E -> { t with e_count = t.e_count + 1 }

    let accumulate_constants_in_expressions ~int_lit_neutral ~int_lit_fold :
        Expr.t list -> t =
      List.fold ~init:(init ~int_lit_neutral ~int_lit_fold)
        ~f:(fun t -> function
        | Expr.Constant c -> accumulate_constant t c
        | expr -> { t with rest_rev = expr :: t.rest_rev })

    let to_expr t ~nullary_constructor ~unary_constructor ~list_constructor
        ~counted_term_constructor =
      let int_lit_term =
        (if t.int_lit_res = t.int_lit_neutral then None
         else Some (Expr.Constant (Int_lit t.int_lit_res)))
        |> Option.to_list
      in
      let pi_term =
        (if t.pi_count = 0 then None
         else if t.pi_count = 1 then Some (Expr.Constant Pi)
         else
           Some
             (counted_term_constructor ~n:(Expr.Constant (Int_lit t.pi_count))
                ~value:(Expr.Constant Pi)))
        |> Option.to_list
      in
      let e_term =
        (if t.e_count = 0 then None
         else if t.e_count = 1 then Some (Expr.Constant E)
         else
           Some
             (counted_term_constructor ~n:(Expr.Constant (Int_lit t.e_count))
                ~value:(Expr.Constant E)))
        |> Option.to_list
      in
      let rest_terms = List.rev t.rest_rev in
      let all_terms = int_lit_term @ pi_term @ e_term @ rest_terms in
      match all_terms with
      | [] -> nullary_constructor ()
      | [ x ] -> unary_constructor x
      | xs -> list_constructor xs
  end

  let rec apply t = function
    | Expr.Constant c -> Expr.Constant c
    | Var v -> Var v
    | Add args ->
        let accumulator =
          Accumulator.accumulate_constants_in_expressions ~int_lit_neutral:0
            ~int_lit_fold:( + ) args
        in
        Accumulator.to_expr accumulator
          ~nullary_constructor:(Fn.const (Expr.Constant (Int_lit 0)))
          ~unary_constructor:Fn.id
          ~list_constructor:(fun xs -> Expr.Add xs)
          ~counted_term_constructor:(fun ~n ~value -> Mul [ n; value ])
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
        let accumulator =
          Accumulator.accumulate_constants_in_expressions ~int_lit_neutral:1
            ~int_lit_fold:( * ) args
        in
        Accumulator.to_expr accumulator
          ~nullary_constructor:(Fn.const (Expr.Constant (Int_lit 1)))
          ~unary_constructor:Fn.id
          ~list_constructor:(fun xs -> Expr.Mul xs)
          ~counted_term_constructor:(fun ~n ~value ->
            Pow { base = value; exponent = n })
    | Frac (num, denom) -> Frac (apply t num, apply t denom)
    | Pow { base; exponent } ->
        Pow { base = apply t base; exponent = apply t exponent }
    | E_pow exponent -> E_pow (apply t exponent)
    | Ln arg -> Ln (apply t arg)
    | Derivative { expr; var } -> Derivative { expr = apply t expr; var }
end

module Take_derivative = struct
  type t = { variable : Variable.t }

  let make variable = { variable }
  let apply { variable } expr = Expr.Derivative { expr; var = variable }
end

module Apply_derivatives = struct
  type t = unit

  let make () = ()

  let rec take_derivative ~var = function
    | Expr.Constant _ -> Expr.Constant (Int_lit 0)
    | Var expr_var ->
        Constant
          (Int_lit
             (if Variable.equal var expr_var then 1
              else
                (* This assumes all variables are independent.
                   Perhaps future iterations will consider functions
                   of variables here, or perhaps these will be implemented elsewhere *)
                0))
    | Add args -> Add (List.map args ~f:(fun arg -> take_derivative ~var arg))
    | Sub (e_pos, e_neg) ->
        Sub (take_derivative ~var e_pos, take_derivative ~var e_neg)
    | Mul args ->
        List.init (List.length args) ~f:(fun i ->
            let arg_i = List.nth_exn args i in
            let arg_i_derivative = take_derivative ~var arg_i in
            let other_args_before = List.take args i in
            let other_args_after = List.drop args (i + 1) in
            Expr.Mul
              (other_args_before @ [ arg_i_derivative ] @ other_args_after))
        |> Add
    | Frac (num, denom) ->
        Frac
          ( Sub
              ( Mul [ take_derivative ~var num; denom ],
                Mul [ num; take_derivative ~var denom ] ),
            Pow { base = denom; exponent = Constant (Int_lit 2) } )
    | Pow { base; exponent } ->
        (* d/dx f(x)^{g(x)} = f(x)^{g(x)} . g(x) . ln( f(x) ) . [ g'(x) . ln( f(x) ) + f'(x) . g(x) / f(x) ] *)
        Mul
          [
            Pow { base; exponent };
            exponent;
            Ln base;
            Add
              [
                Mul [ take_derivative ~var exponent; Ln base ];
                Frac (Mul [ take_derivative ~var base; exponent ], base);
              ];
          ]
    | E_pow exponent -> Mul [ E_pow exponent; take_derivative ~var exponent ]
    | Ln arg -> Frac (take_derivative ~var arg, arg)
    | Derivative { expr; var = new_var } ->
        let expr_derivative = take_derivative ~var:new_var expr in
        take_derivative ~var expr_derivative

  let rec apply t = function
    | Expr.Constant c -> Expr.Constant c
    | Var v -> Var v
    | Add args -> Add (List.map args ~f:(apply t))
    | Sub (e_pos, e_neg) -> Sub (apply t e_pos, apply t e_neg)
    | Mul args -> Mul (List.map args ~f:(apply t))
    | Pow { base; exponent } ->
        Pow { base = apply t base; exponent = apply t exponent }
    | Frac (num, denom) -> Frac (apply t num, apply t denom)
    | E_pow exponent -> E_pow (apply t exponent)
    | Ln arg -> Ln (apply t arg)
    | Derivative { expr; var } -> take_derivative ~var expr
end
