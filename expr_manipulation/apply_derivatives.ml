open! Core
open! Math_assistant

type t = unit

let make () = ()

let rec try_take_derivative ~var = function
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
        (Add (List.map args ~f:(fun arg -> get_derivative_expression ~var arg)))
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
  | Pow { base; exponent } -> (
      (* To make things neater, these are a few hard-coded special cases to try avoid using the most general form *)
      match (base, exponent) with
      | Var base_var, exponent
        when Variable.equal var base_var
             && Set.is_empty (Expr.free_variables exponent) ->
          (* d/dx x^k = k * x^(k-1) *)
          Ok
            (Mul
               [
                 exponent;
                 Pow
                   {
                     base = Var base_var;
                     exponent = Sub (exponent, Constant (Int_lit 1));
                   };
               ])
      | Constant c, exponent ->
          (* d/dx k^(f(x)) = k^(f(x)) * f'(x) * ln k *)
          Ok
            (Mul
               [
                 Pow { base = Constant c; exponent };
                 get_derivative_expression ~var exponent;
                 Ln (Constant c);
               ])
      | base, exponent ->
          (* The most general case: d/dx f(x)^{g(x)} = f(x)^{g(x)} . [ g'(x) . ln( f(x) ) + f'(x) . g(x) / f(x) ] *)
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
               ]))
  | E_pow exponent ->
      Ok (Mul [ E_pow exponent; get_derivative_expression ~var exponent ])
  | Ln arg -> Ok (Frac (get_derivative_expression ~var arg, arg))
  | Derivative { expr; var = new_var } -> (
      match try_take_derivative ~var:new_var expr with
      | Ok expr_derivative -> try_take_derivative ~var expr_derivative
      | Error `Cannot_reduce_due_to_another_variable ->
          Error `Cannot_reduce_due_to_another_variable)

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
