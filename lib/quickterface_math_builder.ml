open! Core
module Math = Quickterface.Math

type t = T of Math.t

let quickterface_math_of_t (T math) = math
let literal s = T (Literal s)
let pi = T Pi
let e = T E
let plus = T Plus
let times = T C_dot

let superscript_power (T base) (T exponent) =
  T (List [ base; Superscript exponent ])

let exponential_single_line (T exponent) = T (List [ Exp; Bracketed exponent ])
let ln (T arg) = T (List [ Ln; Bracketed arg ])
let concat xs = T (List (List.map xs ~f:(fun (T x) -> x)))
let frac (T num) (T den) = T (Frac (num, den))
let bracketed (T x) = T (Bracketed x)

let partial_derivative ~var:(T var) (T expr) =
  T (List [ Frac (Partial, List [ Partial; var ]); expr ])
