open! Core
open Hole_expr

type t = T of Single_hole_expr.t [@@deriving equal, show]

let expr_function_of_t (T hole_expr) =
  Single_hole_expr.expr_function_of_t hole_expr

module Error = struct
  type t =
    | No_matching_command of string
    | Variable_name_error of Sexp.t
    | Cannot_parse_constant_from of string
    | Lexer_error of string
    | No_hole_found
    | Multiple_holes_found
  [@@deriving sexp, equal, show]

  let no_matching_command str = No_matching_command str
  let cannot_parse_constant_from str = Cannot_parse_constant_from str

  module For_testing = struct
    let testable = Alcotest.testable pp equal
  end
end

module Make_parser
    (Helpers : Command_parser_utils_intf.Helpers with module Error := Error) =
struct
  let parse_string string =
    match
      Multihole_expr_parser.main Multihole_expr_lexer.token
        (Lexing.from_string string)
    with
    | result -> Ok result
    | exception Multihole_expr_lexer.Lexer_error err ->
        Error (Error.Lexer_error err)

  let parser string =
    let open Result.Let_syntax in
    let result =
      let%bind multihole_expr = parse_string string in
      let%bind single_hole_expr =
        try
          Ok
            (Single_hole_expr_of_multihole_expr_converter
             .single_hole_expr_of_multihole_expr multihole_expr)
        with
        | Single_hole_expr_of_multihole_expr_converter.No_hole_found ->
            Error Error.No_hole_found
        | Single_hole_expr_of_multihole_expr_converter.Multiple_holes_found ->
            Error Multiple_holes_found
      in
      Ok (T single_hole_expr)
    in
    `Yes_match result

  let all_parsers = [ parser ]
end

module Parser = struct
  module Error' = Error

  include
    Command_parser_utils.Make_parser
      (struct
        type nonrec t = t
      end)
      (Error)
      (Make_parser)

  (* Since I want an extended signature of the [Error] module, I have to overwrite it *)
  module Error = Error'
end

module For_testing = struct
  let of_hole_expr single_hole_expr = T single_hole_expr
  let testable = Alcotest.testable pp equal
end
