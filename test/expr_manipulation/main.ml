open! Core

let () =
  Alcotest.run "Math Assistant"
    [
      ( "Expr Manipulations - Flatten sums and products",
        Test_flatten_sums_and_products.tests );
      ("Expr Manipulations - Reduce Constants", Test_reduce_constants.tests);
      ("Expr Manipulations - Take Derivative", Test_take_derivative.tests);
      ("Expr Manipulations - Apply Derivative", Test_apply_derivatives.tests);
      ( "Expr Manipulations - Evaluate Constant Expressions",
        Test_evaluate_constant_expressions.tests );
    ]
