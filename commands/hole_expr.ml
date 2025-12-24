open! Core

type t = |

let expr_function_of_t (t : t) = match t with _ -> .

module Parser = struct
  type t = unit

  module Error = struct
    type t = Parser_not_implemented [@@deriving sexp, show]
  end

  let make () = ()
  let parse () _ = Error Error.Parser_not_implemented
end
