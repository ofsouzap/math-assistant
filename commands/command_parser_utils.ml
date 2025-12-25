open! Core
open Math_assistant
module Parser = Command_parser_utils_intf.Parser

module Make_helpers (Error : Command_parser_utils_intf.Error) = struct
  let of_regex is_match_regex str ~on_match =
    let re = Re.compile is_match_regex in
    match Re.exec_opt re str with
    | None -> `No_match
    | Some m -> `Yes_match (on_match m)

  let of_regex_string ~pattern = of_regex (Re.Perl.re pattern)

  let parse_constant string =
    let string = string |> String.strip |> String.lowercase in
    if String.equal string "pi" then Ok Constant.pi
    else if String.equal string "e" then Ok Constant.e
    else
      match Int.of_string_opt string with
      | Some n -> Ok (Constant.Int_lit n)
      | None -> Error (Error.cannot_parse_constant_from string)
end

module Make_parser
    (Output : sig
      type t
    end)
    (Error : Command_parser_utils_intf.Error)
    (F : functor
      (_ : Command_parser_utils_intf.Helpers with module Error := Error)
      ->
      Command_parser_utils_intf.Make_parser_functor_output
        with module Error := Error
         and type output := Output.t) =
struct
  module Parsers = F (Make_helpers (Error))

  type t = unit

  let make () = ()

  let parse () string =
    List.fold_result Parsers.all_parsers ~init:() ~f:(fun () parser ->
        match parser string with
        | `No_match -> Ok ()
        | `Yes_match result -> Error result)
    |> function
    | Ok () -> Error (Error.no_matching_command string)
    | Error command_result -> command_result

  module Error = Error
end
