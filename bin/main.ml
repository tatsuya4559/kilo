open Kilo
open Term

let () =
  let disable_raw_mode = enable_raw_mode () in
  Fun.protect (fun () ->
    refresh_screen ();
    process_keypress ())
    ~finally:(fun () -> disable_raw_mode ())
