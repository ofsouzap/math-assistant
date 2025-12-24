open! Core

type t = Variable of string [@@deriving equal, compare, sexp, show]

let quickterface_math_of_t (Variable name) =
  Quickterface_math_builder.literal name

let of_string str =
  match
    String.find str ~f:(fun c -> not (Char.is_alpha c || Char.equal c '_'))
  with
  | Some c ->
      Error
        (Sexp.Atom
           [%string "Invalid character in variable name: %{Char.to_string c}"])
  | None -> Ok (Variable str)

let of_string_exn str =
  match of_string str with
  | Ok var -> var
  | Error err -> failwith (Sexp.to_string_hum err)

module Set = Set.Make (struct
  type t_ = t [@@deriving equal, compare, sexp]
  type t = t_ [@@deriving equal, compare, sexp]

  let compare (Variable v1) (Variable v2) = String.compare v1 v2
end)
