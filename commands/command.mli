(** Well-formed commands from the user *)

open! Core

type t

module Error : sig
  type t [@@deriving sexp, show]
end

val apply :
  t -> Math_assistant.Expr.t -> (Math_assistant.Expr.t, Error.t) Result.t

module Parser : sig
  type command := t
  type t

  module Error : sig
    type t [@@deriving sexp, show]
  end

  val make : unit -> t
  val parse : t -> string -> (command, Error.t) Result.t
end

val parse : string -> (t, Parser.Error.t) Result.t
