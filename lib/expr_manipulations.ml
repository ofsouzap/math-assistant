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
end

module Take_derivative = struct
  type t = { variable : Variable.t }

  let make variable = { variable }
  let apply { variable } expr = Expr.Derivative { expr; var = variable }
end

module Apply_derivatives = struct
  type t = unit

  let make () = ()

  (** Compute the derivative of an expression with respect to a variable.

      When [take_derivative ~var e = Ok e'], this means that [e'] is the
      derivative of [e] w.r.t [var] and [e'] must be reduced or otherwise
      properly transformed. [e'] should never just be [d/d(var) e], as this
      risks non-termination in other functionality.

      When
      [take_derivative ~var e = Error `Cannot_reduce_due_to_another_variable],
      this means that the derivative of [e'] cannot be computed further than
      just expressing it as the derivative of [e] w.r.t. [var]. This happens,
      for example, when [e] is a variable that is different to [var]. *)
  let rec try_take_derivative ~var :
      Expr.t -> (Expr.t, [ `Cannot_reduce_due_to_another_variable ]) Result.t =
    function
    | Expr.Constant _ -> Ok (Expr.Constant (Int_lit 0))
    | Var expr_var ->
        if Variable.equal var expr_var then Ok (Constant (Int_lit 1))
        else
          (* This assumes all variables could be dependent.
                   Perhaps future iterations will specify which
                   variables are functions of which other variables
                   and that will have to be handled here *)
          Error `Cannot_reduce_due_to_another_variable
    | Add args ->
        Ok
          (Add
             (List.map args ~f:(fun arg -> get_derivative_expression ~var arg)))
    | Sub (e_pos, e_neg) ->
        Ok
          (Sub
             ( get_derivative_expression ~var e_pos,
               get_derivative_expression ~var e_neg ))
    | Mul args ->
        Ok
          (List.init (List.length args) ~f:(fun i ->
               let arg_i = List.nth_exn args i in
               let arg_i_derivative = get_derivative_expression ~var arg_i in
               let other_args_before = List.take args i in
               let other_args_after = List.drop args (i + 1) in
               Expr.Mul
                 (other_args_before @ [ arg_i_derivative ] @ other_args_after))
          |> Add)
    | Frac (num, denom) ->
        Ok
          (Frac
             ( Sub
                 ( Mul [ get_derivative_expression ~var num; denom ],
                   Mul [ num; get_derivative_expression ~var denom ] ),
               Pow { base = denom; exponent = Constant (Int_lit 2) } ))
    | Pow { base; exponent } ->
        (* d/dx f(x)^{g(x)} = f(x)^{g(x)} . [ g'(x) . ln( f(x) ) + f'(x) . g(x) / f(x) ] *)
        Ok
          (Mul
             [
               Pow { base; exponent };
               Add
                 [
                   Mul [ get_derivative_expression ~var exponent; Ln base ];
                   Frac
                     ( Mul [ get_derivative_expression ~var base; exponent ],
                       base );
                 ];
             ])
    | E_pow exponent ->
        Ok (Mul [ E_pow exponent; get_derivative_expression ~var exponent ])
    | Ln arg -> Ok (Frac (get_derivative_expression ~var arg, arg))
    | Derivative { expr; var = new_var } -> (
        match try_take_derivative ~var:new_var expr with
        | Ok expr_derivative -> try_take_derivative ~var expr_derivative
        | Error `Cannot_reduce_due_to_another_variable ->
            Error `Cannot_reduce_due_to_another_variable)

  (** Compute the derivative expression of [e] with respect to [var]. This
      function sometimes returns the expression [d/d(var) e], without being able
      to reduce this further *)
  and get_derivative_expression ~var e =
    match try_take_derivative ~var e with
    | Ok res -> res
    | Error `Cannot_reduce_due_to_another_variable ->
        Expr.Derivative { expr = e; var }

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
    | Derivative { expr; var } -> get_derivative_expression ~var expr
end

module Evaluate_constant_expressions = struct
  type t = unit

  let make () = ()

  module Evaluator = struct
    (** Provides safe state manipulation for constant evaluation. Designed to
        use the most useful representation (either using constants as sums or
        products) and keep operations safe *)
    module State = struct
      type t =
        | Plain_constant of Constant.t
            (** Just a single constant, not a compound constant. Values are
                initialized like this to prevent e.g. representing as a constant
                as a sum before realizing that it is used in a product *)
        | Sum_constant of Compound_constant.Summed_constant.t
        | Mul_constant of Compound_constant.Multiplied_constant.t

      let add_neutral_constant = Constant.Int_lit 0
      let add_neutral = Plain_constant add_neutral_constant
      let mul_neutral_constant = Constant.Int_lit 1
      let mul_neutral = Plain_constant mul_neutral_constant
      let t_of_constant c = Plain_constant c

      let sum_exprs_of_t = function
        | Plain_constant c ->
            if Constant.equal c add_neutral_constant then []
            else [ Expr.Constant c ]
        | Sum_constant s -> Compound_constant.Summed_constant.sum_exprs_of_t s
        | Mul_constant m ->
            [ Compound_constant.Multiplied_constant.expr_of_t m ]

      let mul_exprs_of_t = function
        | Plain_constant c ->
            if Constant.equal c mul_neutral_constant then []
            else [ Expr.Constant c ]
        | Sum_constant s -> [ Compound_constant.Summed_constant.expr_of_t s ]
        | Mul_constant m ->
            Compound_constant.Multiplied_constant.mul_exprs_of_t m

      let expr_of_t = function
        | Plain_constant c -> Expr.Constant c
        | Sum_constant s -> Compound_constant.Summed_constant.expr_of_t s
        | Mul_constant m -> Compound_constant.Multiplied_constant.expr_of_t m

      let add t1 t2 =
        match (t1, t2) with
        | Plain_constant c1, Plain_constant c2 ->
            Ok
              (Sum_constant
                 Compound_constant.Summed_constant.(
                   add (t_of_constant c1) (t_of_constant c2)))
        | Plain_constant c, Sum_constant x | Sum_constant x, Plain_constant c ->
            Ok
              (Sum_constant
                 Compound_constant.Summed_constant.(add x (t_of_constant c)))
        | Sum_constant x1, Sum_constant x2 ->
            Ok (Sum_constant Compound_constant.Summed_constant.(add x1 x2))
        | ( Mul_constant mul_constant,
            ((Plain_constant _ | Sum_constant _) as other_constant) )
        | ( ((Plain_constant _ | Sum_constant _) as other_constant),
            Mul_constant mul_constant ) ->
            (* TODO-someday: could look at checking the value of [mul_constant] before
                             assuming we can't add it. E.g. if the [int_lit] is [0]
                             then we are still able to add it to [other_constant]. *)
            Error
              ( other_constant,
                [ Compound_constant.Multiplied_constant.expr_of_t mul_constant ]
              )
        | Mul_constant m1, Mul_constant m2 ->
            Error
              ( add_neutral,
                Compound_constant.Multiplied_constant.
                  [ expr_of_t m1; expr_of_t m2 ] )

      let sub t1 t2 =
        match (t1, t2) with
        | Plain_constant c1, Plain_constant c2 ->
            Ok
              (Sum_constant
                 Compound_constant.Summed_constant.(
                   sub (t_of_constant c1) (t_of_constant c2)))
        | Plain_constant c, Sum_constant x | Sum_constant x, Plain_constant c ->
            Ok
              (Sum_constant
                 Compound_constant.Summed_constant.(sub x (t_of_constant c)))
        | Sum_constant x1, Sum_constant x2 ->
            Ok (Sum_constant Compound_constant.Summed_constant.(sub x1 x2))
        | ( Mul_constant mul_constant,
            ((Plain_constant _ | Sum_constant _) as other_constant) )
        | ( ((Plain_constant _ | Sum_constant _) as other_constant),
            Mul_constant mul_constant ) ->
            (* TODO-someday: as mentioned above, could examine [mul_constant] *)
            Error
              ( other_constant,
                [ Compound_constant.Multiplied_constant.expr_of_t mul_constant ]
              )
        | Mul_constant m1, Mul_constant m2 ->
            Error
              ( add_neutral,
                Compound_constant.Multiplied_constant.
                  [ expr_of_t m1; expr_of_t m2 ] )

      let mul t1 t2 =
        match (t1, t2) with
        | Plain_constant c1, Plain_constant c2 ->
            Ok
              (Mul_constant
                 Compound_constant.Multiplied_constant.(
                   multiply (t_of_constant c1) (t_of_constant c2)))
        | Plain_constant c, Mul_constant x | Mul_constant x, Plain_constant c ->
            Ok
              (Mul_constant
                 Compound_constant.Multiplied_constant.(
                   multiply x (t_of_constant c)))
        | Mul_constant x1, Mul_constant x2 ->
            Ok
              (Mul_constant
                 Compound_constant.Multiplied_constant.(multiply x1 x2))
        | ( Sum_constant sum_constant,
            ((Plain_constant _ | Mul_constant _) as other_constant) )
        | ( ((Plain_constant _ | Mul_constant _) as other_constant),
            Sum_constant sum_constant ) ->
            (* TODO-someday: could look at checking the value of [sum_constant], like described above *)
            Error
              ( other_constant,
                [ Compound_constant.Summed_constant.expr_of_t sum_constant ] )
        | Sum_constant m1, Sum_constant m2 ->
            Error
              ( mul_neutral,
                Compound_constant.Summed_constant.[ expr_of_t m1; expr_of_t m2 ]
              )
    end

    let expr_of_result = function
      | Ok state -> State.expr_of_t state
      | Error e -> e

    (** Evaluate an expression as much as possible.

        When [eval e = Ok s], this means that the expression [e] can be fully
        evaluated to the value [s].

        When [eval e = Error e'], this means that [e] cannot be fully evaluated
        (probably due to the presence of a variable), and that [e'] is [e] with
        subexpressions evaluted as much as possible. *)
    let rec eval : Expr.t -> (State.t, Expr.t) Result.t = function
      | Expr.Constant c -> Ok (State.t_of_constant c)
      | Var v -> Error (Expr.Var v)
      | Add args ->
          eval_list_expr_with_monoid ~monoid_neutral:State.add_neutral
            ~monoid_op:State.add
            ~expr_variant_arg_exprs_of_state:State.sum_exprs_of_t
            ~reconstruct_expr_variant_from_args_list:(fun xs -> Expr.Add xs)
            args
      | Sub (e_pos, e_neg) -> (
          match (eval e_pos, eval e_neg) with
          | Ok e_pos_state, Ok e_neg_state -> (
              match State.sub e_pos_state e_neg_state with
              | Ok res_state -> Ok res_state
              | Error (res_state, rest) ->
                  Error (Add (State.expr_of_t res_state :: rest)))
          | e_pos_res, e_neg_res ->
              Error (Sub (expr_of_result e_pos_res, expr_of_result e_neg_res)))
      | Mul args ->
          eval_list_expr_with_monoid ~monoid_neutral:State.mul_neutral
            ~monoid_op:State.mul
            ~expr_variant_arg_exprs_of_state:State.mul_exprs_of_t
            ~reconstruct_expr_variant_from_args_list:(fun xs -> Expr.Mul xs)
            args
      | Frac (num, denom) ->
          let num' = eval num |> expr_of_result in
          let denom' = eval denom |> expr_of_result in
          Error (Frac (num', denom'))
      | Pow { base; exponent } ->
          let base' = eval base |> expr_of_result in
          let exponent' = eval exponent |> expr_of_result in
          Error (Pow { base = base'; exponent = exponent' })
      | E_pow exponent ->
          let exponent' = eval exponent |> expr_of_result in
          Error (E_pow exponent')
      | Ln arg -> (
          match arg with
          | Constant E -> Ok (State.t_of_constant (Int_lit 1))
          | E_pow exponent | Pow { base = Constant E; exponent } ->
              eval exponent
          | _ ->
              let arg' = eval arg |> expr_of_result in
              Error (Ln arg'))
      | Derivative { expr; var } -> (
          match Apply_derivatives.try_take_derivative ~var expr with
          | Ok expr' -> eval expr'
          | Error `Cannot_reduce_due_to_another_variable ->
              Error (Derivative { expr; var }))

    and eval_list_expr_with_monoid ~monoid_neutral ~monoid_op
        ~expr_variant_arg_exprs_of_state
        ~reconstruct_expr_variant_from_args_list args =
      let res_state, res_rest_rev =
        List.fold args ~init:(monoid_neutral, [])
          ~f:(fun (acc_state, acc_rest_rev) arg ->
            match eval arg with
            | Ok arg_state -> (
                match monoid_op acc_state arg_state with
                | Ok new_acc_state -> (new_acc_state, acc_rest_rev)
                | Error (new_acc_state, extra_rests) ->
                    (new_acc_state, List.rev extra_rests @ acc_rest_rev))
            | Error arg_expr -> (acc_state, arg_expr :: acc_rest_rev))
      in
      match res_rest_rev with
      | [] -> Ok res_state
      | _ :: _ ->
          Error
            (reconstruct_expr_variant_from_args_list
               (expr_variant_arg_exprs_of_state res_state
               @ List.rev res_rest_rev))
  end

  let apply () expr = Evaluator.eval expr |> Evaluator.expr_of_result
end
