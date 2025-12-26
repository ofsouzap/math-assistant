open! Core
open! Math_assistant
open Math_assistant_commands
module Error = Manipulation_command.Parser.Error

let command_testable = Manipulation_command.For_testing.testable

include struct
  open Manipulation_command.For_testing

  let add = add
  let times = times
  let divide_by = divide_by
  let apply_derivatives = apply_derivatives
  let evaluate_constant_expressions = evaluate_constant_expressions
  let take_derivative = take_derivative
end

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
  let parser = Manipulation_command.Parser.make () in
  Manipulation_command.Parser.parse parser input
  |> must_be_ok ~cont:(fun result ->
      Alcotest.check command_testable test_name expected result)

let simple_error_test ~input =
  let parser = Manipulation_command.Parser.make () in
  Manipulation_command.Parser.parse parser input
  |> must_be_error ~cont:(Fn.const ())

let test_add () =
  simple_ok_test ~test_name:"add - int lit" ~input:"add 5"
    ~expected:(add (Int_lit 5));
  simple_ok_test ~test_name:"add - pi" ~input:"add pi" ~expected:(add Pi);
  simple_ok_test ~test_name:"add - e" ~input:"add e" ~expected:(add E)

let test_times () =
  simple_ok_test ~test_name:"times - int lit" ~input:"times 5"
    ~expected:(times (Int_lit 5));
  simple_ok_test ~test_name:"times - pi" ~input:"times pi" ~expected:(times Pi);
  simple_ok_test ~test_name:"times - e" ~input:"times e" ~expected:(times E)

let test_divide_by () =
  simple_ok_test ~test_name:"divide_by - with 'by', int lit"
    ~input:"divide by 5" ~expected:(divide_by (Int_lit 5));
  simple_ok_test ~test_name:"divide_by - without 'by', int lit"
    ~input:"divide 5" ~expected:(divide_by (Int_lit 5));
  simple_ok_test ~test_name:"divide_by - with 'by', pi" ~input:"divide by pi"
    ~expected:(divide_by Pi);
  simple_ok_test ~test_name:"divide_by - without 'by', pi" ~input:"divide pi"
    ~expected:(divide_by Pi);
  simple_ok_test ~test_name:"divide_by - with 'by', e" ~input:"divide by e"
    ~expected:(divide_by E);
  simple_ok_test ~test_name:"divide_by - without 'by', e" ~input:"divide e"
    ~expected:(divide_by E)

let test_apply_derivatives () =
  simple_ok_test ~test_name:"apply deriviatives" ~input:"apply derivatives"
    ~expected:apply_derivatives

let test_evaluate_constant_expressions () =
  simple_ok_test ~test_name:"evaluate constant expressions" ~input:"eval"
    ~expected:evaluate_constant_expressions

let test_take_derivative () =
  simple_ok_test ~test_name:"take derivative" ~input:"derivative x"
    ~expected:(take_derivative (Variable.of_string_exn "x"))

let tests =
  [
    ("add", `Quick, test_add);
    ("times", `Quick, test_times);
    ("divide by", `Quick, test_divide_by);
    ("apply derivatives", `Quick, test_apply_derivatives);
    ("evaluate constant expressions", `Quick, test_evaluate_constant_expressions);
    ("take derivative", `Quick, test_take_derivative);
  ]
