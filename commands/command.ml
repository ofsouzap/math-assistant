open! Core

type t =
  | Manipulation_command of Manipulation_command.t
  | Hole_expr_command of Hole_expr_command.t

module Error = struct
  type t = | [@@deriving sexp, show]
end

let expr_function_of_t = function
  | Manipulation_command cmd -> Manipulation_command.expr_function_of_t cmd
  | Hole_expr_command cmd -> Hole_expr_command.expr_function_of_t cmd

let apply command =
  let expr_function = expr_function_of_t command in
  fun expr -> expr_function expr |> Ok

module Parser = struct
  type t = unit

  module Error = struct
    type t =
      | Error of
          Manipulation_command.Parser.Error.t * Hole_expr_command.Parser.Error.t
    [@@deriving sexp, show]
  end

  let make () = ()

  let parse () str =
    let manipulation_command_parser = Manipulation_command.Parser.make () in
    let hole_command_parser = Hole_expr_command.Parser.make () in
    match
      ( Manipulation_command.Parser.parse manipulation_command_parser str,
        Hole_expr_command.Parser.parse hole_command_parser str )
    with
    | Ok cmd, _ -> Ok (Manipulation_command cmd)
    | _, Ok cmd -> Ok (Hole_expr_command cmd)
    | Error err1, Error err2 -> Error (Error.Error (err1, err2))
end

let parse = Parser.(parse (make ()))
