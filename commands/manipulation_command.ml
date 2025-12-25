open! Core
open Math_assistant

type t =
  | Add of Constant.t
  | Times of Constant.t
  | Divide_by of Constant.t
  | Apply_derivatives
  | Evaluate_constant_expressions
  | Take_derivative of Variable.t

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

module Parser = struct
  type t = unit

  module Error = struct
    type t =
      | No_matching_command of string
      | Incorrect_syntax of { hint : string; encountered_string : string }
      | Variable_name_error of Sexp.t
      | Cannot_parse_constant_from of string
    [@@deriving sexp, show]
  end

  module Command_parsers = struct
    module Helpers = struct
      let of_regex ~is_match_regex str ~on_match =
        let re = Re.compile is_match_regex in
        match Re.exec_opt re str with
        | None -> `No_match
        | Some m -> `Yes_match (on_match m)

      let of_regex_str ~is_match_pattern =
        of_regex ~is_match_regex:(Re.Perl.re is_match_pattern)

      let parse_constant string =
        let string = string |> String.strip |> String.lowercase in
        if String.equal string "pi" then Ok Constant.pi
        else if String.equal string "e" then Ok Constant.e
        else
          match Int.of_string_opt string with
          | Some n -> Ok (Constant.Int_lit n)
          | None -> Error (Error.Cannot_parse_constant_from string)
    end

    (* TODO - add re library as dependency and use that for some of this stuff *)

    let add =
      Helpers.of_regex_str ~is_match_pattern:"^add (.+)$" ~on_match:(fun m ->
          let open Result.Let_syntax in
          let const_str = Re.Group.get m 1 in
          let%map constant = Helpers.parse_constant const_str in
          Add constant)

    let times =
      Helpers.of_regex_str ~is_match_pattern:"^times (.+)$" ~on_match:(fun m ->
          let open Result.Let_syntax in
          let const_str = Re.Group.get m 1 in
          let%map constant = Helpers.parse_constant const_str in
          Times constant)

    let divide_by =
      Helpers.of_regex_str ~is_match_pattern:"^divide( by)? (.+)$"
        ~on_match:(fun m ->
          let open Result.Let_syntax in
          let const_str = Re.Group.get m 2 in
          let%map constant = Helpers.parse_constant const_str in
          Divide_by constant)

    let apply_derivatives =
      Helpers.of_regex_str ~is_match_pattern:"^apply derivatives$"
        ~on_match:(fun _ -> Ok Apply_derivatives)

    let evaluate_constant_expressions =
      Helpers.of_regex_str ~is_match_pattern:"^eval$" ~on_match:(fun _ ->
          Ok Evaluate_constant_expressions)

    let take_derivative =
      Helpers.of_regex_str ~is_match_pattern:"^derivative (.+)$"
        ~on_match:(fun m ->
          let open Result.Let_syntax in
          let const_str = Re.Group.get m 1 in
          let%map var =
            Variable.of_string const_str
            |> Result.map_error ~f:(fun err -> Error.Variable_name_error err)
          in
          Take_derivative var)

    let parsers =
      [
        add;
        times;
        divide_by;
        apply_derivatives;
        evaluate_constant_expressions;
        take_derivative;
      ]
  end

  let make () = ()

  let parse () string =
    List.fold_result Command_parsers.parsers ~init:() ~f:(fun () parser ->
        match parser string with
        | `No_match -> Ok ()
        | `Yes_match result -> Error result)
    |> function
    | Ok () -> Error (Error.No_matching_command string)
    | Error command_result -> command_result
end
