open! Core
open! Math_assistant
include Manipulation.S with type make_args := unit

val try_take_derivative :
  var:Variable.t ->
  Expr.t ->
  (Expr.t, [ `Cannot_reduce_due_to_another_variable ]) Result.t
(** Compute the derivative of an expression with respect to a variable.

    When [take_derivative ~var e = Ok e'], this means that [e'] is the
    derivative of [e] w.r.t [var] and [e'] must be reduced or otherwise properly
    transformed. [e'] should never just be [d/d(var) e], as this risks
    non-termination in other functionality.

    When
    [take_derivative ~var e = Error `Cannot_reduce_due_to_another_variable],
    this means that the derivative of [e'] cannot be computed further than just
    expressing it as the derivative of [e] w.r.t. [var]. This happens, for
    example, when [e] is a variable that is different to [var]. *)

val get_derivative_expression : var:Variable.t -> Expr.t -> Expr.t
(** Compute the derivative expression of [e] with respect to [var]. This
    function sometimes returns the expression [d/d(var) e], without being able
    to reduce this further *)
