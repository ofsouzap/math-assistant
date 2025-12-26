open! Core

let () =
  Alcotest.run "Commands"
    [
      ("Commands - Hole Expression Command", Test_hole_expression_command.tests);
    ]
