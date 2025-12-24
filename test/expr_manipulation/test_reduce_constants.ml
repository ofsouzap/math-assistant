open! Core
open! Math_assistant
module R = Math_assistant_expr_manipulation.Reduce_constants

(* Used a lot of AI for test writing *)

let expr_testable = Expr.For_testing.testable
let int_lit x = Expr.Constant (Constant.Int_lit x)
let pi = Expr.Constant Constant.Pi
let e = Expr.Constant Constant.E
let var varname = Expr.Var (Variable.of_string_exn varname)

let test_reduce_int_lits_addition () =
  let reducer = R.make () in

  (* Test: 2 + 3 should become 5 *)
  let expr = Expr.Add [ int_lit 2; int_lit 3 ] in
  let result = R.apply reducer expr in
  Alcotest.(check expr_testable) "2 + 3 = 5" (int_lit 5) result;

  (* Test: 1 + 2 + 3 should become 6 *)
  let expr = Expr.Add [ int_lit 1; int_lit 2; int_lit 3 ] in
  let result = R.apply reducer expr in
  Alcotest.(check expr_testable) "1 + 2 + 3 = 6" (int_lit 6) result

let test_reduce_int_lits_multiplication () =
  let reducer = R.make () in

  (* Test: 2 * 3 should become 6 *)
  let expr = Expr.Mul [ int_lit 2; int_lit 3 ] in
  let result = R.apply reducer expr in
  Alcotest.(check expr_testable) "2 * 3 = 6" (int_lit 6) result;

  (* Test: 2 * 3 * 4 should become 24 *)
  let expr = Expr.Mul [ int_lit 2; int_lit 3; int_lit 4 ] in
  let result = R.apply reducer expr in
  Alcotest.(check expr_testable) "2 * 3 * 4 = 24" (int_lit 24) result

let test_reduce_pi_addition () =
  let reducer = R.make () in

  (* Test: pi + pi should become 2*pi *)
  let expr = Expr.Add [ pi; pi ] in
  let result = R.apply reducer expr in
  let expected = Expr.Mul [ int_lit 2; pi ] in
  Alcotest.(check expr_testable) "pi + pi = 2*pi" expected result;

  (* Test: pi + pi + pi should become 3*pi *)
  let expr = Expr.Add [ pi; pi; pi ] in
  let result = R.apply reducer expr in
  let expected = Expr.Mul [ int_lit 3; pi ] in
  Alcotest.(check expr_testable) "pi + pi + pi = 3*pi" expected result

let test_reduce_pi_multiplication () =
  let reducer = R.make () in

  (* Test: pi * pi should become pi^2 *)
  let expr = Expr.Mul [ pi; pi ] in
  let result = R.apply reducer expr in
  let expected = Expr.Pow { base = pi; exponent = int_lit 2 } in
  Alcotest.(check expr_testable) "pi * pi = pi^2" expected result;

  (* Test: pi * pi * pi should become pi^3 *)
  let expr = Expr.Mul [ pi; pi; pi ] in
  let result = R.apply reducer expr in
  let expected = Expr.Pow { base = pi; exponent = int_lit 3 } in
  Alcotest.(check expr_testable) "pi * pi * pi = pi^3" expected result

let test_reduce_e_addition () =
  let reducer = R.make () in

  (* Test: e + e should become 2*e *)
  let expr = Expr.Add [ e; e ] in
  let result = R.apply reducer expr in
  let expected = Expr.Mul [ int_lit 2; e ] in
  Alcotest.(check expr_testable) "e + e = 2*e" expected result;

  (* Test: e + e + e should become 3*e *)
  let expr = Expr.Add [ e; e; e ] in
  let result = R.apply reducer expr in
  let expected = Expr.Mul [ int_lit 3; e ] in
  Alcotest.(check expr_testable) "e + e + e = 3*e" expected result

let test_reduce_e_multiplication () =
  let reducer = R.make () in

  (* Test: e * e should become e^2 *)
  let expr = Expr.Mul [ e; e ] in
  let result = R.apply reducer expr in
  let expected = Expr.Pow { base = e; exponent = int_lit 2 } in
  Alcotest.(check expr_testable) "e * e = e^2" expected result;

  (* Test: e * e * e should become e^3 *)
  let expr = Expr.Mul [ e; e; e ] in
  let result = R.apply reducer expr in
  let expected = Expr.Pow { base = e; exponent = int_lit 3 } in
  Alcotest.(check expr_testable) "e * e * e = e^3" expected result

let test_reduce_combined_addition () =
  let reducer = R.make () in
  let var_x = var "x" in

  (* Test: 2 + 3 + pi + pi + e + x should become 5 + 2*pi + e + x *)
  let expr = Expr.Add [ int_lit 2; int_lit 3; pi; pi; e; var_x ] in
  let result = R.apply reducer expr in
  let expected = Expr.Add [ int_lit 5; Expr.Mul [ int_lit 2; pi ]; e; var_x ] in
  Alcotest.(check expr_testable)
    "2 + 3 + pi + pi + e + x = 5 + 2*pi + e + x" expected result;

  (* Test: 1 + pi + 2 + e + pi + 3 + e + e should become 6 + 2*pi + 3*e *)
  let expr = Expr.Add [ int_lit 1; pi; int_lit 2; e; pi; int_lit 3; e; e ] in
  let result = R.apply reducer expr in
  let expected =
    Expr.Add
      [ int_lit 6; Expr.Mul [ int_lit 2; pi ]; Expr.Mul [ int_lit 3; e ] ]
  in
  Alcotest.(check expr_testable)
    "mixed addition reduces with correct order" expected result

let test_reduce_combined_multiplication () =
  let reducer = R.make () in
  let var_x = var "x" in

  (* Test: 2 * 3 * pi * pi * e * x should become 6 * pi^2 * e * x *)
  let expr = Expr.Mul [ int_lit 2; int_lit 3; pi; pi; e; var_x ] in
  let result = R.apply reducer expr in
  let expected =
    Expr.Mul
      [ int_lit 6; Expr.Pow { base = pi; exponent = int_lit 2 }; e; var_x ]
  in
  Alcotest.(check expr_testable)
    "2 * 3 * pi * pi * e * x = 6 * pi^2 * e * x" expected result;

  (* Test: 2 * pi * 3 * e * pi * e * e should become 6 * pi^2 * e^3 *)
  let expr = Expr.Mul [ int_lit 2; pi; int_lit 3; e; pi; e; e ] in
  let result = R.apply reducer expr in
  let expected =
    Expr.Mul
      [
        int_lit 6;
        Expr.Pow { base = pi; exponent = int_lit 2 };
        Expr.Pow { base = e; exponent = int_lit 3 };
      ]
  in
  Alcotest.(check expr_testable)
    "mixed multiplication reduces with correct order" expected result

let tests =
  [
    ("reduce int lits addition", `Quick, test_reduce_int_lits_addition);
    ( "reduce int lits multiplication",
      `Quick,
      test_reduce_int_lits_multiplication );
    ("reduce pi addition", `Quick, test_reduce_pi_addition);
    ("reduce pi multiplication", `Quick, test_reduce_pi_multiplication);
    ("reduce e addition", `Quick, test_reduce_e_addition);
    ("reduce e multiplication", `Quick, test_reduce_e_multiplication);
    ("reduce combined addition", `Quick, test_reduce_combined_addition);
    ( "reduce combined multiplication",
      `Quick,
      test_reduce_combined_multiplication );
  ]
