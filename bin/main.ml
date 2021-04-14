open Kilo
open Term

let () =
  let disable_raw_mode = enable_raw_mode () in
  Fun.protect (fun () ->
    let editor_config =
      match Editor_config.create () with
      | None -> exit 1
      | Some x -> x
    in
    Editor_config.refresh_screen editor_config;
    process_keypress ())
    ~finally:(fun () -> disable_raw_mode ())
