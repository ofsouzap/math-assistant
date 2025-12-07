open! Core

let () =
  Alcotest.run "Math Assistant"
    [ ("Expr Manipulations", Test_expr_manipulations.tests) ]
