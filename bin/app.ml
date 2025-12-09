open! Core

module App (Io : Quickterface.Io.S) = struct
  let main ~io () =
    let%lwt () = Io.print_text io "Hello, world!" () in
    Lwt.return ()
end
