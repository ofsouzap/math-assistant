open! Core
open Math_assistant

module type Error = sig
  type t [@@deriving sexp, show]

  val cannot_parse_constant_from : string -> t
  val no_matching_command : string -> t
end

module Parser = struct
  module type S = sig
    module Error : Error

    type output
    type t

    val make : unit -> t
    val parse : t -> string -> (output, Error.t) Result.t
  end
end

module type Helpers = sig
  module Error : Error

  val of_regex :
    Re.t ->
    string ->
    on_match:(Re.Group.t -> 'a) ->
    [ `Yes_match of 'a | `No_match ]

  val of_regex_string :
    pattern:string ->
    string ->
    on_match:(Re.Group.t -> 'a) ->
    [ `Yes_match of 'a | `No_match ]

  val parse_constant : string -> (Constant.t, Error.t) Result.t
end

module type Make_parser_functor_output = sig
  module Error : Error

  type output

  val all_parsers :
    (string -> [ `Yes_match of (output, Error.t) Result.t | `No_match ]) list
end

module type S = sig
  module Parser : module type of Parser
  module Make_helpers (Error : Error) : Helpers with module Error := Error

  module Make_parser
      (Output : sig
        type t
      end)
      (Error : Error)
      (_ : functor
        (_ : Helpers with module Error := Error)
        ->
        Make_parser_functor_output
          with module Error := Error
           and type output := Output.t) :
    Parser.S with module Error = Error and type output := Output.t
end
