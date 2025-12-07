open! Core

type t =
  | Literal of string
  | Concat of t list
  | Command of string * t list
  | Wrapped of { left_char : string; right_char : string; content : t }

let rec to_string = function
  | Literal s -> s
  | Concat xs -> String.concat ~sep:"" (List.map xs ~f:to_string)
  | Wrapped { left_char; right_char; content } ->
      sprintf "\\left%s %s \\right%s" left_char (to_string content) right_char
  | Command (name, args) ->
      let args_str =
        String.concat ~sep:""
          (List.map args ~f:(fun arg_t -> sprintf "{%s}" (to_string arg_t)))
      in
      sprintf "\\%s%s" name args_str

let literal s = Literal s
let pi = literal "\\pi"
let e = literal "e"
let concat xs = Concat xs
let command name args = Command (name, args)

let left_right_wrapper left_char right_char content =
  Wrapped { left_char; right_char; content }

let parens = left_right_wrapper "(" ")"
let braces = left_right_wrapper "{" "}"
let square_brackets = left_right_wrapper "[" "]"

let rec depth = function
  | Literal _ -> 0
  | Concat xs ->
      List.map ~f:depth xs
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
  | Wrapped { content; _ } -> 1 + depth content
  | Command (_, args) ->
      List.map ~f:depth args
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
