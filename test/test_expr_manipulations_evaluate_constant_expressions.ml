open! Core
open Math_assistant
module E = Expr_manipulations.Evaluate_constant_expressions

let evaluator = E.make ()

(* Used a lot of AI for test writing *)

let expr_testable = Expr.For_testing.testable
let int_lit x = Expr.Constant (Constant.Int_lit x)
let pi = Expr.Constant Constant.Pi
let e = Expr.Constant Constant.E
let var varname = Expr.Var (Variable.Variable varname)

(* Tests for full evaluation of constant expressions *)

let test_evaluate_int_lits_addition () =
  (* Test: 2 + 3 should become 5 *)
  let expr = Expr.Add [ int_lit 2; int_lit 3 ] in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "2 + 3 = 5" (int_lit 5) result;

  (* Test: 1 + 2 + 3 should become 6 *)
  let expr = Expr.Add [ int_lit 1; int_lit 2; int_lit 3 ] in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "1 + 2 + 3 = 6" (int_lit 6) result

let test_evaluate_pi_addition () =
  (* Test: pi + pi should become 2*pi *)
  let expr = Expr.Add [ pi; pi ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Mul [ int_lit 2; pi ] in
  Alcotest.(check expr_testable) "pi + pi = 2*pi" expected result;

  (* Test: pi + pi + pi should become 3*pi *)
  let expr = Expr.Add [ pi; pi; pi ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Mul [ int_lit 3; pi ] in
  Alcotest.(check expr_testable) "pi + pi + pi = 3*pi" expected result

let test_evaluate_e_addition () =
  (* Test: e + e should become 2*e *)
  let expr = Expr.Add [ e; e ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Mul [ int_lit 2; e ] in
  Alcotest.(check expr_testable) "e + e = 2*e" expected result;

  (* Test: e + e + e should become 3*e *)
  let expr = Expr.Add [ e; e; e ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Mul [ int_lit 3; e ] in
  Alcotest.(check expr_testable) "e + e + e = 3*e" expected result

let test_evaluate_combined_addition () =
  let var_x = var "x" in

  (* Test: 2 + 3 + pi + pi + e + x should evaluate constants *)
  let expr = Expr.Add [ int_lit 2; int_lit 3; pi; pi; e; var_x ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Add [ int_lit 5; Expr.Mul [ int_lit 2; pi ]; e; var_x ] in
  Alcotest.(check expr_testable)
    "2 + 3 + pi + pi + e + x evaluates to 5 + 2*pi + e + x" expected result;

  (* Test: 1 + pi + 2 + e + pi + 3 + e + e *)
  let expr = Expr.Add [ int_lit 1; pi; int_lit 2; e; pi; int_lit 3; e; e ] in
  let result = E.apply evaluator expr in
  let expected =
    Expr.Add
      [ int_lit 6; Expr.Mul [ int_lit 2; pi ]; Expr.Mul [ int_lit 3; e ] ]
  in
  Alcotest.(check expr_testable)
    "mixed addition evaluates correctly" expected result

(* Tests for Ln evaluation *)

let test_evaluate_ln_of_e () =
  (* Test: ln(e) should become 1 *)
  let expr = Expr.Ln e in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "ln(e) = 1" (int_lit 1) result

let test_evaluate_ln_of_e_pow () =
  (* Test: ln(e^x) should become x *)
  let var_x = var "x" in
  let expr = Expr.Ln (Expr.E_pow var_x) in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "ln(e^x) = x" var_x result;

  (* Test: ln(e^(2+3)) should become 5 *)
  let expr = Expr.Ln (Expr.E_pow (Expr.Add [ int_lit 2; int_lit 3 ])) in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "ln(e^(2+3)) = 5" (int_lit 5) result

let test_evaluate_ln_of_pow_e () =
  (* Test: ln(e^x) using Pow form should become x *)
  let var_x = var "x" in
  let expr = Expr.Ln (Expr.Pow { base = e; exponent = var_x }) in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "ln(e^x) = x (Pow form)" var_x result

(* Tests for partial evaluation - when variables are present *)

let test_partial_evaluation_addition_with_vars () =
  let var_x = var "x" in
  let var_y = var "y" in

  (* Test: x + 2 + 3 should become x + 5 *)
  let expr = Expr.Add [ var_x; int_lit 2; int_lit 3 ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Add [ int_lit 5; var_x ] in
  Alcotest.(check expr_testable) "x + 2 + 3 = x + 5" expected result;

  (* Test: x + y + pi + pi + 1 + 2 *)
  let expr = Expr.Add [ var_x; var_y; pi; pi; int_lit 1; int_lit 2 ] in
  let result = E.apply evaluator expr in
  let expected =
    Expr.Add [ int_lit 3; Expr.Mul [ int_lit 2; pi ]; var_x; var_y ]
  in
  Alcotest.(check expr_testable)
    "partial evaluation with multiple variables" expected result

let test_partial_evaluation_nested () =
  let var_x = var "x" in

  (* Test: (2 + 3) + (4 + 5) + x should become 14 + x *)
  let expr =
    Expr.Add
      [
        Expr.Add [ int_lit 2; int_lit 3 ];
        Expr.Add [ int_lit 4; int_lit 5 ];
        var_x;
      ]
  in
  let result = E.apply evaluator expr in
  let expected = Expr.Add [ int_lit 14; var_x ] in
  Alcotest.(check expr_testable) "nested additions evaluate" expected result

(* Tests for Frac - partial evaluation only *)

let test_partial_evaluation_frac () =
  let var_x = var "x" in

  (* Test: (2 + 3) / (4 + 5) should evaluate numerator and denominator *)
  let expr =
    Expr.Frac
      (Expr.Add [ int_lit 2; int_lit 3 ], Expr.Add [ int_lit 4; int_lit 5 ])
  in
  let result = E.apply evaluator expr in
  let expected = Expr.Frac (int_lit 5, int_lit 9) in
  Alcotest.(check expr_testable) "frac evaluates subexpressions" expected result;

  (* Test: x / (2 + 3) should evaluate denominator *)
  let expr = Expr.Frac (var_x, Expr.Add [ int_lit 2; int_lit 3 ]) in
  let result = E.apply evaluator expr in
  let expected = Expr.Frac (var_x, int_lit 5) in
  Alcotest.(check expr_testable) "frac evaluates denominator" expected result

(* Tests for Pow - partial evaluation only *)

let test_partial_evaluation_pow () =
  let var_x = var "x" in

  (* Test: (2 + 3)^(4 + 5) should evaluate base and exponent *)
  let expr =
    Expr.Pow
      {
        base = Expr.Add [ int_lit 2; int_lit 3 ];
        exponent = Expr.Add [ int_lit 4; int_lit 5 ];
      }
  in
  let result = E.apply evaluator expr in
  let expected = Expr.Pow { base = int_lit 5; exponent = int_lit 9 } in
  Alcotest.(check expr_testable) "pow evaluates subexpressions" expected result;

  (* Test: x^(2 + 3) should evaluate exponent *)
  let expr =
    Expr.Pow { base = var_x; exponent = Expr.Add [ int_lit 2; int_lit 3 ] }
  in
  let result = E.apply evaluator expr in
  let expected = Expr.Pow { base = var_x; exponent = int_lit 5 } in
  Alcotest.(check expr_testable) "pow evaluates exponent" expected result

(* Tests for E_pow - partial evaluation only *)

let test_partial_evaluation_e_pow () =
  let var_x = var "x" in

  (* Test: e^(2 + 3) should evaluate exponent *)
  let expr = Expr.E_pow (Expr.Add [ int_lit 2; int_lit 3 ]) in
  let result = E.apply evaluator expr in
  let expected = Expr.E_pow (int_lit 5) in
  Alcotest.(check expr_testable) "e_pow evaluates exponent" expected result;

  (* Test: e^(x + 2 + 3) should evaluate constant part *)
  let expr = Expr.E_pow (Expr.Add [ var_x; int_lit 2; int_lit 3 ]) in
  let result = E.apply evaluator expr in
  let expected = Expr.E_pow (Expr.Add [ int_lit 5; var_x ]) in
  Alcotest.(check expr_testable)
    "e_pow evaluates constant part of exponent" expected result

(* Tests for Ln - partial evaluation *)

let test_partial_evaluation_ln () =
  let var_x = var "x" in

  (* Test: ln(2 + 3) should evaluate argument *)
  let expr = Expr.Ln (Expr.Add [ int_lit 2; int_lit 3 ]) in
  let result = E.apply evaluator expr in
  let expected = Expr.Ln (int_lit 5) in
  Alcotest.(check expr_testable) "ln evaluates argument" expected result;

  (* Test: ln(x + 2 + 3) should evaluate constant part *)
  let expr = Expr.Ln (Expr.Add [ var_x; int_lit 2; int_lit 3 ]) in
  let result = E.apply evaluator expr in
  let expected = Expr.Ln (Expr.Add [ int_lit 5; var_x ]) in
  Alcotest.(check expr_testable)
    "ln evaluates constant part of argument" expected result

(* Tests for Derivative - should be evaluated *)

let test_evaluate_derivative () =
  let var_x = Variable.Variable "x" in

  (* Test: d/dx(x) should become 1 *)
  let expr = Expr.Derivative { expr = var "x"; var = var_x } in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "d/dx(x) = 1" (int_lit 1) result;

  (* Test: d/dx(2 + 3) should become 0 *)
  let expr =
    Expr.Derivative { expr = Expr.Add [ int_lit 2; int_lit 3 ]; var = var_x }
  in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "d/dx(2 + 3) = 0" (int_lit 0) result;

  (* Test: d/dx(x + 2 + 3) should become 1 (derivative of sum is sum of derivatives) *)
  let expr =
    Expr.Derivative
      { expr = Expr.Add [ var "x"; int_lit 2; int_lit 3 ]; var = var_x }
  in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "d/dx(x + 2 + 3) = 1" (int_lit 1) result

(* Tests for Sub - full and partial evaluation *)

let test_evaluate_sub_int_lits () =
  (* Test: 5 - 3 should become 2 *)
  let expr = Expr.Sub (int_lit 5, int_lit 3) in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "5 - 3 = 2" (int_lit 2) result;

  (* Test: 10 - 7 should become 3 *)
  let expr = Expr.Sub (int_lit 10, int_lit 7) in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "10 - 7 = 3" (int_lit 3) result

let test_evaluate_sub_constants () =
  (* Test: pi - pi should become 0 *)
  let expr = Expr.Sub (pi, pi) in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "pi - pi = 0" (int_lit 0) result;

  (* Test: e - e should become 0 *)
  let expr = Expr.Sub (e, e) in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "e - e = 0" (int_lit 0) result

let test_partial_evaluation_sub () =
  let var_x = var "x" in

  (* Test: (2 + 3) - (1 + 1) should evaluate to 5 - 2 = 3 *)
  let expr =
    Expr.Sub
      (Expr.Add [ int_lit 2; int_lit 3 ], Expr.Add [ int_lit 1; int_lit 1 ])
  in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "(2 + 3) - (1 + 1) = 3" (int_lit 3) result;

  (* Test: x - (2 + 3) should evaluate denominator to x - 5 *)
  let expr = Expr.Sub (var_x, Expr.Add [ int_lit 2; int_lit 3 ]) in
  let result = E.apply evaluator expr in
  let expected = Expr.Sub (var_x, int_lit 5) in
  Alcotest.(check expr_testable) "x - (2 + 3) = x - 5" expected result;

  (* Test: (x + 1 + 2) - (y + 3 + 4) should evaluate constants *)
  let var_y = var "y" in
  let expr =
    Expr.Sub
      ( Expr.Add [ var_x; int_lit 1; int_lit 2 ],
        Expr.Add [ var_y; int_lit 3; int_lit 4 ] )
  in
  let result = E.apply evaluator expr in
  let expected =
    Expr.Sub (Expr.Add [ int_lit 3; var_x ], Expr.Add [ int_lit 7; var_y ])
  in
  Alcotest.(check expr_testable)
    "(x + 1 + 2) - (y + 3 + 4) evaluates constants" expected result

(* Tests for Mul - full and partial evaluation *)

let test_evaluate_mul_int_lits () =
  (* Test: 2 * 3 should become 6 *)
  let expr = Expr.Mul [ int_lit 2; int_lit 3 ] in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "2 * 3 = 6" (int_lit 6) result;

  (* Test: 2 * 3 * 4 should become 24 *)
  let expr = Expr.Mul [ int_lit 2; int_lit 3; int_lit 4 ] in
  let result = E.apply evaluator expr in
  Alcotest.(check expr_testable) "2 * 3 * 4 = 24" (int_lit 24) result

let test_evaluate_mul_pi () =
  (* Test: pi * pi should become pi^2 *)
  let expr = Expr.Mul [ pi; pi ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Pow { base = pi; exponent = int_lit 2 } in
  Alcotest.(check expr_testable) "pi * pi = pi^2" expected result;

  (* Test: pi * pi * pi should become pi^3 *)
  let expr = Expr.Mul [ pi; pi; pi ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Pow { base = pi; exponent = int_lit 3 } in
  Alcotest.(check expr_testable) "pi * pi * pi = pi^3" expected result

let test_evaluate_mul_e () =
  (* Test: e * e should become e^2 *)
  let expr = Expr.Mul [ e; e ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Pow { base = e; exponent = int_lit 2 } in
  Alcotest.(check expr_testable) "e * e = e^2" expected result;

  (* Test: e * e * e should become e^3 *)
  let expr = Expr.Mul [ e; e; e ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Pow { base = e; exponent = int_lit 3 } in
  Alcotest.(check expr_testable) "e * e * e = e^3" expected result

let test_evaluate_mul_combined () =
  let var_x = var "x" in

  (* Test: 2 * 3 * pi * pi * e * x should become 6 * pi^2 * e * x *)
  let expr = Expr.Mul [ int_lit 2; int_lit 3; pi; pi; e; var_x ] in
  let result = E.apply evaluator expr in
  let expected =
    Expr.Mul
      [ int_lit 6; Expr.Pow { base = pi; exponent = int_lit 2 }; e; var_x ]
  in
  Alcotest.(check expr_testable)
    "2 * 3 * pi * pi * e * x = 6 * pi^2 * e * x" expected result;

  (* Test: 2 * pi * 3 * e * pi * e * e should become 6 * pi^2 * e^3 *)
  let expr = Expr.Mul [ int_lit 2; pi; int_lit 3; e; pi; e; e ] in
  let result = E.apply evaluator expr in
  let expected =
    Expr.Mul
      [
        int_lit 6;
        Expr.Pow { base = pi; exponent = int_lit 2 };
        Expr.Pow { base = e; exponent = int_lit 3 };
      ]
  in
  Alcotest.(check expr_testable)
    "mixed multiplication evaluates correctly" expected result

let test_partial_evaluation_mul () =
  let var_x = var "x" in
  let var_y = var "y" in

  (* Test: x * (2 * 3) should become x * 6 *)
  let expr = Expr.Mul [ var_x; Expr.Mul [ int_lit 2; int_lit 3 ] ] in
  let result = E.apply evaluator expr in
  let expected = Expr.Mul [ int_lit 6; var_x ] in
  Alcotest.(check expr_testable) "x * (2 * 3) = x * 6" expected result;

  (* Test: (x * y) * (2 * 3 * 4) should evaluate nested constants *)
  let expr =
    Expr.Mul
      [
        Expr.Mul [ var_x; var_y ]; Expr.Mul [ int_lit 2; int_lit 3; int_lit 4 ];
      ]
  in
  let result = E.apply evaluator expr in
  let expected = Expr.Mul [ int_lit 24; Mul [ var_x; var_y ] ] in
  Alcotest.(check expr_testable)
    "(x * y) * (2 * 3 * 4) = 24 * (x * y)" expected result

let tests =
  [
    ("evaluate int lits addition", `Quick, test_evaluate_int_lits_addition);
    ("evaluate pi addition", `Quick, test_evaluate_pi_addition);
    ("evaluate e addition", `Quick, test_evaluate_e_addition);
    ("evaluate combined addition", `Quick, test_evaluate_combined_addition);
    ("evaluate ln(e)", `Quick, test_evaluate_ln_of_e);
    ("evaluate ln(e^x)", `Quick, test_evaluate_ln_of_e_pow);
    ("evaluate ln(e^x) Pow form", `Quick, test_evaluate_ln_of_pow_e);
    ( "partial evaluation addition with vars",
      `Quick,
      test_partial_evaluation_addition_with_vars );
    ("partial evaluation nested", `Quick, test_partial_evaluation_nested);
    ("partial evaluation frac", `Quick, test_partial_evaluation_frac);
    ("partial evaluation pow", `Quick, test_partial_evaluation_pow);
    ("partial evaluation e_pow", `Quick, test_partial_evaluation_e_pow);
    ("partial evaluation ln", `Quick, test_partial_evaluation_ln);
    ("evaluate derivative", `Quick, test_evaluate_derivative);
    ("evaluate sub int lits", `Quick, test_evaluate_sub_int_lits);
    ("evaluate sub constants", `Quick, test_evaluate_sub_constants);
    ("partial evaluation sub", `Quick, test_partial_evaluation_sub);
    ("evaluate mul int lits", `Quick, test_evaluate_mul_int_lits);
    ("evaluate mul pi", `Quick, test_evaluate_mul_pi);
    ("evaluate mul e", `Quick, test_evaluate_mul_e);
    ("evaluate mul combined", `Quick, test_evaluate_mul_combined);
    ("partial evaluation mul", `Quick, test_partial_evaluation_mul);
  ]
