open! Core
open! Math_assistant

type t = unit

let make () = ()

module Evaluator = struct
  (** Provides safe state manipulation for constant evaluation. Designed to use
      the most useful representation (either using constants as sums or
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
      | Mul_constant m -> [ Compound_constant.Multiplied_constant.expr_of_t m ]

    let mul_exprs_of_t = function
      | Plain_constant c ->
          if Constant.equal c mul_neutral_constant then []
          else [ Expr.Constant c ]
      | Sum_constant s -> [ Compound_constant.Summed_constant.expr_of_t s ]
      | Mul_constant m -> Compound_constant.Multiplied_constant.mul_exprs_of_t m

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
            (Mul_constant Compound_constant.Multiplied_constant.(multiply x1 x2))
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
        | E_pow exponent | Pow { base = Constant E; exponent } -> eval exponent
        | _ ->
            let arg' = eval arg |> expr_of_result in
            Error (Ln arg'))
    | Derivative { expr; var } -> (
        match Apply_derivatives.try_take_derivative ~var expr with
        | Ok expr' -> eval expr'
        | Error `Cannot_reduce_due_to_another_variable ->
            Error (Derivative { expr; var }))

  and eval_list_expr_with_monoid ~monoid_neutral ~monoid_op
      ~expr_variant_arg_exprs_of_state ~reconstruct_expr_variant_from_args_list
      args =
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
             (expr_variant_arg_exprs_of_state res_state @ List.rev res_rest_rev))
end

let apply () expr = Evaluator.eval expr |> Evaluator.expr_of_result
