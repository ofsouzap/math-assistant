open! Core
open Math_assistant
module D = Expr_manipulations.Apply_derivatives

(* Used a lot of AI for test writing *)

let expr_testable = Expr.For_testing.testable
let int_lit x = Expr.Constant (Constant.Int_lit x)
let pi = Expr.Constant Constant.Pi
let e = Expr.Constant Constant.E
let var varname = Expr.Var (Variable.Variable varname)

let test_derivative_of_constant () =
  let manipulation = D.make () in

  (* Test: apply d/dx(5) = 0 *)
  let expr =
    Expr.Derivative { expr = int_lit 5; var = Variable.Variable "x" }
  in
  let result = D.apply manipulation expr in
  let expected = int_lit 0 in
  Alcotest.(check expr_testable) "derivative of constant" expected result

let test_derivative_of_variable () =
  let manipulation = D.make () in

  (* Test: apply d/dx(x) = 1 *)
  let expr = Expr.Derivative { expr = var "x"; var = Variable.Variable "x" } in
  let result = D.apply manipulation expr in
  let expected = int_lit 1 in
  Alcotest.(check expr_testable) "derivative of variable" expected result;

  (* Test: apply d/dx(y) = d/dx(y) (remains unevaluated) *)
  let expr = Expr.Derivative { expr = var "y"; var = Variable.Variable "x" } in
  let result = D.apply manipulation expr in
  let expected =
    Expr.Derivative { expr = var "y"; var = Variable.Variable "x" }
  in
  Alcotest.(check expr_testable)
    "derivative of different variable" expected result

let test_derivative_of_sum () =
  let manipulation = D.make () in

  (* Test: apply d/dt(x + y) = d/dt(x) + d/dt(y) *)
  let expr =
    Expr.Derivative
      { expr = Expr.Add [ var "x"; var "y" ]; var = Variable.Variable "t" }
  in
  let result = D.apply manipulation expr in
  let expected =
    Expr.Add
      [
        Expr.Derivative { expr = var "x"; var = Variable.Variable "t" };
        Expr.Derivative { expr = var "y"; var = Variable.Variable "t" };
      ]
  in
  Alcotest.(check expr_testable) "derivative of sum" expected result

let test_derivative_of_product () =
  let manipulation = D.make () in

  (* Test: apply d/dx(x * y) = 1 * y + x * d/dx(y) *)
  let expr =
    Expr.Derivative
      { expr = Expr.Mul [ var "x"; var "y" ]; var = Variable.Variable "x" }
  in
  let result = D.apply manipulation expr in
  let expected =
    Expr.Add
      [
        Mul [ int_lit 1; var "y" ];
        Mul
          [
            var "x";
            Expr.Derivative { expr = var "y"; var = Variable.Variable "x" };
          ];
      ]
  in
  Alcotest.(check expr_testable) "derivative of product" expected result

let test_derivative_of_power_general_case () =
  let manipulation = D.make () in

  (* Test: apply d/dt( (2*x)^(y) ) = (2x)^(y) * ( dy/dt * ln(2x) + ((0 * x + 2 * dx/dt) * y) / (2x) ) *)
  let expr =
    Expr.Derivative
      {
        expr =
          Expr.Pow { base = Mul [ int_lit 2; var "x" ]; exponent = var "y" };
        var = Variable.Variable "t";
      }
  in
  let result = D.apply manipulation expr in
  let expected =
    Expr.Mul
      [
        Pow { base = Mul [ int_lit 2; var "x" ]; exponent = var "y" };
        Add
          [
            Mul
              [
                Expr.Derivative { expr = var "y"; var = Variable.Variable "t" };
                Ln (Mul [ int_lit 2; var "x" ]);
              ];
            Frac
              ( Mul
                  [
                    Add
                      [
                        Mul [ int_lit 0; var "x" ];
                        Mul
                          [
                            int_lit 2;
                            Expr.Derivative
                              { expr = var "x"; var = Variable.Variable "t" };
                          ];
                      ];
                    var "y";
                  ],
                Mul [ int_lit 2; var "x" ] );
          ];
      ]
  in
  Alcotest.(check expr_testable)
    "derivative of power general case" expected result

let test_derivative_of_power_variable_base_constant_exponent () =
  let manipulation = D.make () in

  (* Test: apply d/dx(x^3) = 3 * x^(3-1) *)
  let expr =
    Expr.Derivative
      {
        expr = Expr.Pow { base = var "x"; exponent = int_lit 3 };
        var = Variable.Variable "x";
      }
  in
  let result = D.apply manipulation expr in
  let expected =
    Expr.Mul
      [
        int_lit 3; Pow { base = var "x"; exponent = Sub (int_lit 3, int_lit 1) };
      ]
  in
  Alcotest.(check expr_testable)
    "derivative of x^k (special case)" expected result

let test_derivative_of_power_constant_base_variable_exponent () =
  let manipulation = D.make () in

  (* Test: apply d/dx(2^x) = 2^x * 1 * ln(2) *)
  let expr =
    Expr.Derivative
      {
        expr = Expr.Pow { base = int_lit 2; exponent = var "x" };
        var = Variable.Variable "x";
      }
  in
  let result = D.apply manipulation expr in
  let expected =
    Expr.Mul
      [
        Pow { base = int_lit 2; exponent = var "x" }; int_lit 1; Ln (int_lit 2);
      ]
  in
  Alcotest.(check expr_testable)
    "derivative of k^x (special case)" expected result;

  (* Test: apply d/dx(pi^(y)) = pi^(y) * d/dx(y) * ln(pi) *)
  let expr2 =
    Expr.Derivative
      {
        expr = Expr.Pow { base = pi; exponent = var "y" };
        var = Variable.Variable "x";
      }
  in
  let result2 = D.apply manipulation expr2 in
  let expected2 =
    Expr.Mul
      [
        Pow { base = pi; exponent = var "y" };
        Expr.Derivative { expr = var "y"; var = Variable.Variable "x" };
        Ln pi;
      ]
  in
  Alcotest.(check expr_testable)
    "derivative of k^(f(x)) (special case)" expected2 result2

let test_derivative_of_nested_derivative () =
  let manipulation = D.make () in

  (* Test: apply d/dx(d/dy(x)) - should apply outer derivative *)
  let inner = Expr.Derivative { expr = var "x"; var = Variable.Variable "y" } in
  let expr = Expr.Derivative { expr = inner; var = Variable.Variable "x" } in
  let result = D.apply manipulation expr in
  let expected =
    Expr.Derivative
      {
        expr = Expr.Derivative { expr = var "x"; var = Variable.Variable "y" };
        var = Variable.Variable "x";
      }
  in
  Alcotest.(check expr_testable) "derivative of derivative" expected result

let test_derivative_respects_variable () =
  let manipulation = D.make () in

  let expr_x =
    Expr.Derivative { expr = var "z"; var = Variable.Variable "x" }
  in
  let expr_y =
    Expr.Derivative { expr = var "z"; var = Variable.Variable "y" }
  in

  let result_x = D.apply manipulation expr_x in
  let result_y = D.apply manipulation expr_y in

  let expected_x =
    Expr.Derivative { expr = var "z"; var = Variable.Variable "x" }
  in
  let expected_y =
    Expr.Derivative { expr = var "z"; var = Variable.Variable "y" }
  in

  Alcotest.(check expr_testable)
    "derivative with respect to x" expected_x result_x;
  Alcotest.(check expr_testable)
    "derivative with respect to y" expected_y result_y

let tests =
  [
    ("derivative of constant", `Quick, test_derivative_of_constant);
    ("derivative of variable", `Quick, test_derivative_of_variable);
    ("derivative of sum", `Quick, test_derivative_of_sum);
    ("derivative of product", `Quick, test_derivative_of_product);
    ("derivative of power", `Quick, test_derivative_of_power_general_case);
    ( "derivative of power (variable base, constant exponent)",
      `Quick,
      test_derivative_of_power_variable_base_constant_exponent );
    ( "derivative of power (constant base, variable exponent)",
      `Quick,
      test_derivative_of_power_constant_base_variable_exponent );
    ( "derivative of nested derivative",
      `Quick,
      test_derivative_of_nested_derivative );
    ("derivative respects variable", `Quick, test_derivative_respects_variable);
  ]
