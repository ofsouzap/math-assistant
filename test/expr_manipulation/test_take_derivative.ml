open! Core
open! Math_assistant
module D = Math_assistant_expr_manipulation.Take_derivative

(* Used a lot of AI for test writing *)

let expr_testable = Expr.For_testing.testable
let int_lit x = Expr.Constant (Constant.Int_lit x)
let pi = Expr.Constant Constant.Pi
let e = Expr.Constant Constant.E
let var varname = Expr.Var (Variable.Variable varname)

let test_derivative_of_constant () =
  let derivative = D.make (Variable.Variable "x") in

  (* Test: d/dx(5) creates Derivative expression *)
  let expr = int_lit 5 in
  let result = D.apply derivative expr in
  let expected =
    Expr.Derivative { expr = int_lit 5; var = Variable.Variable "x" }
  in
  Alcotest.(check expr_testable) "derivative of constant" expected result

let test_derivative_of_variable () =
  let derivative = D.make (Variable.Variable "x") in

  (* Test: d/dx(x) creates Derivative expression *)
  let expr = var "x" in
  let result = D.apply derivative expr in
  let expected =
    Expr.Derivative { expr = var "x"; var = Variable.Variable "x" }
  in
  Alcotest.(check expr_testable) "derivative of variable" expected result;

  (* Test: d/dx(y) creates Derivative expression *)
  let expr = var "y" in
  let result = D.apply derivative expr in
  let expected =
    Expr.Derivative { expr = var "y"; var = Variable.Variable "x" }
  in
  Alcotest.(check expr_testable)
    "derivative of different variable" expected result

let test_derivative_of_sum () =
  let derivative = D.make (Variable.Variable "t") in

  (* Test: d/dt(x + y) creates Derivative expression *)
  let expr = Expr.Add [ var "x"; var "y" ] in
  let result = D.apply derivative expr in
  let expected =
    Expr.Derivative
      { expr = Expr.Add [ var "x"; var "y" ]; var = Variable.Variable "t" }
  in
  Alcotest.(check expr_testable) "derivative of sum" expected result

let test_derivative_of_product () =
  let derivative = D.make (Variable.Variable "x") in

  (* Test: d/dx(x * y) creates Derivative expression *)
  let expr = Expr.Mul [ var "x"; var "y" ] in
  let result = D.apply derivative expr in
  let expected =
    Expr.Derivative
      { expr = Expr.Mul [ var "x"; var "y" ]; var = Variable.Variable "x" }
  in
  Alcotest.(check expr_testable) "derivative of product" expected result

let test_derivative_of_power () =
  let derivative = D.make (Variable.Variable "x") in

  (* Test: d/dx(x^2) creates Derivative expression *)
  let expr = Expr.Pow { base = var "x"; exponent = int_lit 2 } in
  let result = D.apply derivative expr in
  let expected =
    Expr.Derivative
      {
        expr = Expr.Pow { base = var "x"; exponent = int_lit 2 };
        var = Variable.Variable "x";
      }
  in
  Alcotest.(check expr_testable) "derivative of power" expected result

let test_derivative_of_nested_derivative () =
  let derivative = D.make (Variable.Variable "x") in

  (* Test: d/dx(d/dy(x)) creates nested Derivative expression *)
  let inner = Expr.Derivative { expr = var "x"; var = Variable.Variable "y" } in
  let result = D.apply derivative inner in
  let expected =
    Expr.Derivative { expr = inner; var = Variable.Variable "x" }
  in
  Alcotest.(check expr_testable) "derivative of derivative" expected result

let test_derivative_respects_variable () =
  let derivative_x = D.make (Variable.Variable "x") in
  let derivative_y = D.make (Variable.Variable "y") in

  let expr = var "z" in
  let result_x = D.apply derivative_x expr in
  let result_y = D.apply derivative_y expr in

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
