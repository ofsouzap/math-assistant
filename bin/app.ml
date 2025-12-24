open! Core
open Math_assistant

module App (Io : Quickterface.Io.S) = struct
  type empty = |

  module State = struct
    type t = T of Expr.t

    let init = T (Expr.Constant Constant.zero)
    let get_expr (T expr) = expr
    let make ~expr = T expr

    let print t ~io =
      let expr = get_expr t in
      Io.print_math io
        (Expr.quickterface_math_of_t expr
        |> Quickterface_math_builder.quickterface_math_of_t)
  end

  let main_loop () ~io : empty Lwt.t =
    let rec loop curr_state =
      let open Math_assistant_commands in
      let%lwt () = State.print curr_state ~io () in
      let%lwt input = Io.read_text io () in
      let%lwt new_state =
        match Command.parse input with
        | Ok command -> (
            match Command.apply command (State.get_expr curr_state) with
            | Error error ->
                let%lwt () = Io.print_text io (Command.Error.show error) () in
                Lwt.return curr_state
            | Ok new_expr -> State.make ~expr:new_expr |> Lwt.return)
        | Error error ->
            let%lwt () =
              Io.print_text io (Command.Parser.Error.show error) ()
            in
            Lwt.return curr_state
      in
      loop new_state
    in
    loop State.init

  let main ~io () =
    let%lwt _ = main_loop ~io () in
    assert false
end
