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

    let add string =
      let prefix = "add " in
      if String.is_prefix string ~prefix then
        let const_str =
          String.drop_prefix string (String.length prefix) |> String.strip
        in
        match Helpers.parse_constant const_str with
        | Ok constant -> `Yes_match (Ok (Add constant))
        | Error err -> `Yes_match (Error err)
      else `No_match

    let times string =
      let prefix = "times " in
      if String.is_prefix string ~prefix then
        let const_str =
          String.drop_prefix string (String.length prefix) |> String.strip
        in
        match Helpers.parse_constant const_str with
        | Ok constant -> `Yes_match (Ok (Times constant))
        | Error err -> `Yes_match (Error err)
      else `No_match

    let divide_by string =
      let prefix = "divide by " in
      if String.is_prefix string ~prefix then
        let const_str =
          String.drop_prefix string (String.length prefix) |> String.strip
        in
        match Helpers.parse_constant const_str with
        | Ok constant -> `Yes_match (Ok (Divide_by constant))
        | Error err -> `Yes_match (Error err)
      else `No_match

    let apply_derivatives string =
      if String.(equal (lowercase string) "apply derivatives") then
        `Yes_match (Ok Apply_derivatives)
      else `No_match

    let evaluate_constant_expressions string =
      if String.(equal (lowercase string) "eval") then
        `Yes_match (Ok Evaluate_constant_expressions)
      else `No_match

    let take_derivative string =
      let prefix = "derivative " in
      if String.is_prefix string ~prefix then
        let var_name =
          String.drop_prefix string (String.length prefix) |> String.strip
        in
        match Variable.of_string var_name with
        | Ok variable -> `Yes_match (Ok (Take_derivative variable))
        | Error err -> `Yes_match (Error (Error.Variable_name_error err))
      else `No_match

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
