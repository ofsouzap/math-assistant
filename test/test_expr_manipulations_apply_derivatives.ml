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

let test_derivative_of_power () =
  let manipulation = D.make () in

  (* Test: apply d/dx(x^2) = x^2 * (0 * ln x + (1 * 2) / (x)) *)
  let expr =
    Expr.Derivative
      {
        expr = Expr.Pow { base = var "x"; exponent = int_lit 2 };
        var = Variable.Variable "x";
      }
  in
  let result = D.apply manipulation expr in
  let expected =
    Expr.Mul
      [
        Expr.Pow { base = var "x"; exponent = int_lit 2 };
        Expr.Add
          [
            Mul [ int_lit 0; Ln (var "x") ];
            Frac (Mul [ int_lit 1; int_lit 2 ], var "x");
          ];
      ]
  in
  Alcotest.(check expr_testable) "derivative of power" expected result

let test_derivative_of_expression_power () =
  let manipulation = D.make () in

  (* Test: apply d/dt( (2*x)^(y) ) = (2x)^(y) * ( dy/dt * ln(2x) + (2 * dx/dt * y) / (2x) ) *)
  let expr =
    Expr.Derivative
      {
        expr =
          Expr.Pow
            { base = Mul [ int_lit 2; var "x" ]; exponent = Mul [ var "y" ] };
        var = Variable.Variable "t";
      }
  in
  let result = D.apply manipulation expr in
  let expected =
    Expr.Mul
      [
        Pow { base = Mul [ int_lit 2; var "x" ]; exponent = Mul [ var "y" ] };
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
                    int_lit 2;
                    Expr.Derivative
                      { expr = var "x"; var = Variable.Variable "t" };
                    var "y";
                  ],
                Mul [ int_lit 2; var "x" ] );
          ];
      ]
  in
  Alcotest.(check expr_testable) "derivative of power" expected result

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
    ("derivative of power", `Quick, test_derivative_of_power);
    ( "derivative of nested derivative",
      `Quick,
      test_derivative_of_nested_derivative );
    ("derivative respects variable", `Quick, test_derivative_respects_variable);
  ]
