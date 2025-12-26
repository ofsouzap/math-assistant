open! Core
open Math_assistant

type t =
  | Add of Constant.t
  | Times of Constant.t
  | Divide_by of Constant.t
  | Apply_derivatives
  | Evaluate_constant_expressions
  | Take_derivative of Variable.t
[@@deriving equal, show]

let expr_function_of_t =
  let open Math_assistant_expr_manipulation in
  function
  | Add constant -> fun expr -> Expr.Add [ expr; Expr.Constant constant ]
  | Times constant -> fun expr -> Expr.Mul [ expr; Expr.Constant constant ]
  | Divide_by constant -> fun expr -> Expr.Frac (expr, Expr.Constant constant)
  | Apply_derivatives -> Apply_derivatives.(apply (make ()))
  | Evaluate_constant_expressions ->
      Evaluate_constant_expressions.(apply (make ()))
  | Take_derivative variable -> Take_derivative.(apply (make variable))

module Error = struct
  type t =
    | No_matching_command of string
    | Incorrect_syntax of { hint : string; encountered_string : string }
    | Variable_name_error of Sexp.t
    | Cannot_parse_constant_from of string
  [@@deriving sexp, equal, show]

  let no_matching_command str = No_matching_command str
  let cannot_parse_constant_from str = Cannot_parse_constant_from str
end

module Make_parser
    (Helpers : Command_parser_utils_intf.Helpers with module Error := Error) =
struct
  let add =
    Helpers.of_regex_string ~pattern:"^add (.+)$" ~on_match:(fun m ->
        let open Result.Let_syntax in
        let const_str = Re.Group.get m 1 in
        let%map constant = Helpers.parse_constant const_str in
        Add constant)

  let times =
    Helpers.of_regex_string ~pattern:"^times (.+)$" ~on_match:(fun m ->
        let open Result.Let_syntax in
        let const_str = Re.Group.get m 1 in
        let%map constant = Helpers.parse_constant const_str in
        Times constant)

  let divide_by =
    Helpers.of_regex_string ~pattern:"^divide( by)? (.+)$" ~on_match:(fun m ->
        let open Result.Let_syntax in
        let const_str = Re.Group.get m 2 in
        let%map constant = Helpers.parse_constant const_str in
        Divide_by constant)

  let apply_derivatives =
    Helpers.of_regex_string ~pattern:"^apply derivatives$" ~on_match:(fun _ ->
        Ok Apply_derivatives)

  let evaluate_constant_expressions =
    Helpers.of_regex_string ~pattern:"^eval$" ~on_match:(fun _ ->
        Ok Evaluate_constant_expressions)

  let take_derivative =
    Helpers.of_regex_string ~pattern:"^derivative (.+)$" ~on_match:(fun m ->
        let open Result.Let_syntax in
        let const_str = Re.Group.get m 1 in
        let%map var =
          Variable.of_string const_str
          |> Result.map_error ~f:(fun err -> Error.Variable_name_error err)
        in
        Take_derivative var)

  let all_parsers =
    [
      add;
      times;
      divide_by;
      apply_derivatives;
      evaluate_constant_expressions;
      take_derivative;
    ]
end

module Parser =
  Command_parser_utils.Make_parser
    (struct
      type nonrec t = t
    end)
    (Error)
    (Make_parser)

module For_testing = struct
  let add c = Add c
  let times c = Times c
  let divide_by c = Divide_by c
  let apply_derivatives = Apply_derivatives
  let evaluate_constant_expressions = Evaluate_constant_expressions
  let take_derivative var = Take_derivative var
  let testable = Alcotest.testable pp equal
end
