open! Core

module Command_name = struct
  type t = Command_name of string
end

module Arg = struct
  type t = String of string
end

module Command = struct
  type t = { name : Command_name.t; args : Arg.t list }
end
