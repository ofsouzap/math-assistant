open! Core
open! Math_assistant
module F = Math_assistant_expr_manipulation.Flatten_sums_and_products

(* Used a lot of AI for test writing *)

let expr_testable = Expr.For_testing.testable
let int_lit x = Expr.Constant (Constant.Int_lit x)
let pi = Expr.Constant Constant.Pi
let e = Expr.Constant Constant.E
let var varname = Expr.Var (Variable.Variable varname)

let test_flatten_nested_addition () =
  let flattener = F.make () in

  (* Test: (1 + 2) + 3 should become 1 + 2 + 3 *)
  let expr = Expr.Add [ Expr.Add [ int_lit 1; int_lit 2 ]; int_lit 3 ] in
  let result = F.apply flattener expr in
  let expected = Expr.Add [ int_lit 1; int_lit 2; int_lit 3 ] in
  Alcotest.(check expr_testable) "(1 + 2) + 3 flattens" expected result;

  (* Test: 1 + (2 + 3) should become 1 + 2 + 3 *)
  let expr = Expr.Add [ int_lit 1; Expr.Add [ int_lit 2; int_lit 3 ] ] in
  let result = F.apply flattener expr in
  let expected = Expr.Add [ int_lit 1; int_lit 2; int_lit 3 ] in
  Alcotest.(check expr_testable) "1 + (2 + 3) flattens" expected result

let test_flatten_nested_multiplication () =
  let flattener = F.make () in

  (* Test: (2 * 3) * 4 should become 2 * 3 * 4 *)
  let expr = Expr.Mul [ Expr.Mul [ int_lit 2; int_lit 3 ]; int_lit 4 ] in
  let result = F.apply flattener expr in
  let expected = Expr.Mul [ int_lit 2; int_lit 3; int_lit 4 ] in
  Alcotest.(check expr_testable) "(2 * 3) * 4 flattens" expected result;

  (* Test: 2 * (3 * 4) should become 2 * 3 * 4 *)
  let expr = Expr.Mul [ int_lit 2; Expr.Mul [ int_lit 3; int_lit 4 ] ] in
  let result = F.apply flattener expr in
  let expected = Expr.Mul [ int_lit 2; int_lit 3; int_lit 4 ] in
  Alcotest.(check expr_testable) "2 * (3 * 4) flattens" expected result

let test_flatten_deeply_nested_addition () =
  let flattener = F.make () in

  (* Test: ((1 + 2) + (3 + 4)) + 5 should become 1 + 2 + 3 + 4 + 5 *)
  let expr =
    Expr.Add
      [
        Expr.Add
          [
            Expr.Add [ int_lit 1; int_lit 2 ]; Expr.Add [ int_lit 3; int_lit 4 ];
          ];
        int_lit 5;
      ]
  in
  let result = F.apply flattener expr in
  let expected =
    Expr.Add [ int_lit 1; int_lit 2; int_lit 3; int_lit 4; int_lit 5 ]
  in
  Alcotest.(check expr_testable)
    "deeply nested addition flattens" expected result

let test_flatten_deeply_nested_multiplication () =
  let flattener = F.make () in

  (* Test: ((2 * 3) * (4 * 5)) * 6 should become 2 * 3 * 4 * 5 * 6 *)
  let expr =
    Expr.Mul
      [
        Expr.Mul
          [
            Expr.Mul [ int_lit 2; int_lit 3 ]; Expr.Mul [ int_lit 4; int_lit 5 ];
          ];
        int_lit 6;
      ]
  in
  let result = F.apply flattener expr in
  let expected =
    Expr.Mul [ int_lit 2; int_lit 3; int_lit 4; int_lit 5; int_lit 6 ]
  in
  Alcotest.(check expr_testable)
    "deeply nested multiplication flattens" expected result

let test_flatten_mixed_constants () =
  let flattener = F.make () in
  let var_x = var "x" in

  (* Test: (pi + e) + (x + pi) should become pi + e + x + pi *)
  let expr = Expr.Add [ Expr.Add [ pi; e ]; Expr.Add [ var_x; pi ] ] in
  let result = F.apply flattener expr in
  let expected = Expr.Add [ pi; e; var_x; pi ] in
  Alcotest.(check expr_testable) "mixed addition flattens" expected result;

  (* Test: (pi * e) * (x * pi) should become pi * e * x * pi *)
  let expr = Expr.Mul [ Expr.Mul [ pi; e ]; Expr.Mul [ var_x; pi ] ] in
  let result = F.apply flattener expr in
  let expected = Expr.Mul [ pi; e; var_x; pi ] in
  Alcotest.(check expr_testable) "mixed multiplication flattens" expected result

let test_flatten_non_nested_unchanged () =
  let flattener = F.make () in
  let var_x = var "x" in

  (* Test: flat addition stays flat *)
  let expr = Expr.Add [ int_lit 1; pi; var_x ] in
  let result = F.apply flattener expr in
  Alcotest.(check expr_testable) "flat addition unchanged" expr result;

  (* Test: flat multiplication stays flat *)
  let expr = Expr.Mul [ int_lit 2; e; var_x ] in
  let result = F.apply flattener expr in
  Alcotest.(check expr_testable) "flat multiplication unchanged" expr result

let test_flatten_preserves_other_operations () =
  let flattener = F.make () in

  (* Test: Power operations are preserved *)
  let expr = Expr.Pow { base = pi; exponent = int_lit 2 } in
  let result = F.apply flattener expr in
  Alcotest.(check expr_testable) "power operation preserved" expr result;

  (* Test: Fraction operations are preserved *)
  let expr = Expr.Frac (int_lit 1, int_lit 2) in
  let result = F.apply flattener expr in
  Alcotest.(check expr_testable) "fraction operation preserved" expr result

let tests =
  [
    ("flatten nested addition", `Quick, test_flatten_nested_addition);
    ("flatten nested multiplication", `Quick, test_flatten_nested_multiplication);
    ( "flatten deeply nested addition",
      `Quick,
      test_flatten_deeply_nested_addition );
    ( "flatten deeply nested multiplication",
      `Quick,
      test_flatten_deeply_nested_multiplication );
    ("flatten mixed constants", `Quick, test_flatten_mixed_constants);
    ("flatten non-nested unchanged", `Quick, test_flatten_non_nested_unchanged);
    ( "flatten preserves other operations",
      `Quick,
      test_flatten_preserves_other_operations );
  ]
