open! Core

let () =
  Alcotest.run "Math Assistant"
    [
      ("Expr Manipulations - Take Derivative", Test_take_derivative.tests);
      ("Expr Manipulations - Apply Derivative", Test_apply_derivatives.tests);
      ( "Expr Manipulations - Evaluate Constant Expressions",
        Test_evaluate_constant_expressions.tests );
    ]
