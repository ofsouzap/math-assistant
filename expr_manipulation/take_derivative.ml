open! Core
open! Math_assistant

type t = { variable : Variable.t }

let make variable = { variable }
let apply { variable } expr = Expr.Derivative { expr; var = variable }
