open! Core
open! Math_assistant
open Math_assistant_commands
module Error = Hole_expr_command.Parser.Error

let command_testable = Hole_expr_command.For_testing.testable
let error_testable = Error.For_testing.testable
let make_command = Hole_expr_command.For_testing.of_hole_expr

let must_be_ok ?fmt_error x ~cont =
  match x with
  | Ok x' -> cont x'
  | Error err -> (
      match fmt_error with
      | None -> Alcotest.fail "Result value was an error but expected to be ok"
      | Some fmt_error -> Alcotest.failf "%s" (fmt_error err))

let must_be_error ?fmt_ok x ~cont =
  match x with
  | Error err -> cont err
  | Ok x -> (
      match fmt_ok with
      | None -> Alcotest.fail "Result value was an ok but expected to be error"
      | Some fmt_ok -> Alcotest.failf "%s" (fmt_ok x))

let simple_ok_test ~test_name ~input ~expected =
  let parser = Hole_expr_command.Parser.make () in
  Hole_expr_command.Parser.parse parser input
  |> must_be_ok ~cont:(fun result ->
      Alcotest.check command_testable test_name (make_command expected) result)

let simple_error_test ~test_name ~input ~expected =
  let parser = Hole_expr_command.Parser.make () in
  Hole_expr_command.Parser.parse parser input
  |> must_be_error ~cont:(fun result ->
      Alcotest.check error_testable test_name expected result)

let test_identity () =
  simple_ok_test ~test_name:"identity" ~input:"_" ~expected:Hole

let test_add () =
  simple_ok_test ~test_name:"add - right, int lit" ~input:"5 + _"
    ~expected:(Add ([ Constant (Int_lit 5) ], Hole, []));
  simple_ok_test ~test_name:"add - left, int lit" ~input:"_ + 5"
    ~expected:(Add ([], Hole, [ Constant (Int_lit 5) ]));
  simple_ok_test ~test_name:"add - left, var" ~input:"_ + x"
    ~expected:(Add ([], Hole, [ Var (Variable.of_string_exn "x") ]))

let test_constant () =
  simple_ok_test ~test_name:"constant - int lit, positive" ~input:"_ + 5"
    ~expected:(Add ([], Hole, [ Constant (Int_lit 5) ]));
  simple_ok_test ~test_name:"constant - int lit, negative" ~input:"_ + -5"
    ~expected:(Add ([], Hole, [ Constant (Int_lit (-5)) ]));
  simple_ok_test ~test_name:"constant - e" ~input:"_ + e"
    ~expected:(Add ([], Hole, [ Constant E ]));
  simple_ok_test ~test_name:"constant - pi" ~input:"_ + pi"
    ~expected:(Add ([], Hole, [ Constant Pi ]))

let test_var () =
  simple_ok_test ~test_name:"variable - single char name" ~input:"_ + x"
    ~expected:(Add ([], Hole, [ Var (Variable.of_string_exn "x") ]));
  simple_ok_test ~test_name:"variable - long name" ~input:"_ + myvar"
    ~expected:(Add ([], Hole, [ Var (Variable.of_string_exn "myvar") ]));
  simple_ok_test ~test_name:"variable - starting with 'e'" ~input:"_ + ex"
    ~expected:(Add ([], Hole, [ Var (Variable.of_string_exn "ex") ]));
  simple_ok_test ~test_name:"variable - named 'p'" ~input:"_ + p"
    ~expected:(Add ([], Hole, [ Var (Variable.of_string_exn "p") ]));
  simple_ok_test ~test_name:"variable - starting with 'p'" ~input:"_ + px"
    ~expected:(Add ([], Hole, [ Var (Variable.of_string_exn "px") ]));
  simple_ok_test ~test_name:"variable - starting with \"pi\"" ~input:"_ + pix"
    ~expected:(Add ([], Hole, [ Var (Variable.of_string_exn "pix") ]))

let test_sub () =
  simple_ok_test ~test_name:"sub - right" ~input:"5 - _"
    ~expected:(Sub_right (Constant (Int_lit 5), Hole));
  simple_ok_test ~test_name:"sub - left" ~input:"_ - 5"
    ~expected:(Sub_left (Hole, Constant (Int_lit 5)))

let test_mul () =
  simple_ok_test ~test_name:"mul - right, int lit" ~input:"5 * _"
    ~expected:(Mul ([ Constant (Int_lit 5) ], Hole, []));
  simple_ok_test ~test_name:"mul - left, int lit" ~input:"_ * 5"
    ~expected:(Mul ([], Hole, [ Constant (Int_lit 5) ]));
  simple_ok_test ~test_name:"mul - left, var" ~input:"_ * x"
    ~expected:(Mul ([], Hole, [ Var (Variable.of_string_exn "x") ]))

let test_frac () =
  simple_ok_test ~test_name:"frac - denominator" ~input:"5 / _"
    ~expected:(Frac_denominator (Constant (Int_lit 5), Hole));
  simple_ok_test ~test_name:"frac - numerator" ~input:"_ / 5"
    ~expected:(Frac_numerator (Hole, Constant (Int_lit 5)))

let test_pow () =
  simple_ok_test ~test_name:"pow - exponent" ~input:"5 ^ _"
    ~expected:(Pow_exponent (Constant (Int_lit 5), Hole));
  simple_ok_test ~test_name:"pow - base" ~input:"_ ^ 5"
    ~expected:(Pow_base (Hole, Constant (Int_lit 5)))

let test_ln () =
  simple_ok_test ~test_name:"ln" ~input:"ln _" ~expected:(Ln Hole)

let test_no_holes () =
  simple_error_test ~test_name:"no holes - flat" ~input:"2 + 6"
    ~expected:Error.No_hole_found;
  simple_error_test ~test_name:"no holes - nested" ~input:"(1 * 5) + 9"
    ~expected:Error.No_hole_found

let test_multiple_holes () =
  simple_error_test ~test_name:"multiple holes - flat" ~input:"_ + _"
    ~expected:Error.Multiple_holes_found;
  simple_error_test ~test_name:"multiple holes - nested" ~input:"(_ * 5) + _"
    ~expected:Error.Multiple_holes_found

let test_flattening () =
  simple_ok_test ~test_name:"flattening - add only" ~input:"1 + 2 + _ + 4 + 5"
    ~expected:
      (Add
         ( [ Constant (Int_lit 1); Constant (Int_lit 2) ],
           Hole,
           [ Constant (Int_lit 4); Constant (Int_lit 5) ] ));
  simple_ok_test ~test_name:"flattening - mul only" ~input:"1 * 2 * _ * 4 * 5"
    ~expected:
      (Mul
         ( [ Constant (Int_lit 1); Constant (Int_lit 2) ],
           Hole,
           [ Constant (Int_lit 4); Constant (Int_lit 5) ] ));
  simple_ok_test ~test_name:"flattening - add with compound in terms"
    ~input:"(1 * 3) + 2 + _ + 4 + 5"
    ~expected:
      (Add
         ( [
             Mul [ Constant (Int_lit 1); Constant (Int_lit 3) ];
             Constant (Int_lit 2);
           ],
           Hole,
           [ Constant (Int_lit 4); Constant (Int_lit 5) ] ));
  simple_ok_test ~test_name:"flattening - add with compound in hole"
    ~input:"1 + 2 + (_ * 2) + 4 + 5"
    ~expected:
      (Add
         ( [ Constant (Int_lit 1); Constant (Int_lit 2) ],
           Mul ([], Hole, [ Constant (Int_lit 2) ]),
           [ Constant (Int_lit 4); Constant (Int_lit 5) ] ))

let test_compound_tests () =
  simple_ok_test ~test_name:"compound tests - 1" ~input:"(1 + _) * ln 2"
    ~expected:
      (Mul
         ( [],
           Add ([ Constant (Int_lit 1) ], Hole, []),
           [ Ln (Constant (Int_lit 2)) ] ));
  simple_ok_test ~test_name:"compound tests - 2"
    ~input:"ln (1 + (2 * x) + (_ * y^2))"
    ~expected:
      (Ln
         (Add
            ( [
                Constant (Int_lit 1);
                Mul [ Constant (Int_lit 2); Var (Variable.of_string_exn "x") ];
              ],
              Mul
                ( [],
                  Hole,
                  [
                    Pow
                      {
                        base = Var (Variable.of_string_exn "y");
                        exponent = Constant (Int_lit 2);
                      };
                  ] ),
              [] )))

let tests =
  [
    ("identity", `Quick, test_identity);
    ("add", `Quick, test_add);
    ("constant", `Quick, test_constant);
    ("var", `Quick, test_var);
    ("sub", `Quick, test_sub);
    ("mul", `Quick, test_mul);
    ("frac", `Quick, test_frac);
    ("pow", `Quick, test_pow);
    ("ln", `Quick, test_ln);
    ("no holes", `Quick, test_no_holes);
    ("multiple holes", `Quick, test_multiple_holes);
    ("flattening", `Quick, test_flattening);
    ("compound tests", `Quick, test_compound_tests);
  ]
