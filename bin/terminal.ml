open! Core

module App =
  Quickterface_terminal_app.Terminal_app_intf.Make (Math_assistant_app.App.App)

let () = Lwt_main.run (App.run ())
