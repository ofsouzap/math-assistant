open! Core

let () =
  Alcotest.run "Math Assistant"
    [
      ( "Expr Manipulations - Flatten sums and products",
        Test_expr_manipulations_flatten_sums_and_products.tests );
      ( "Expr Manipulations - Reduce Constants",
        Test_expr_manipulations_reduce_constants.tests );
    ]
