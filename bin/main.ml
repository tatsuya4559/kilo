open Kilo

let () =
  let disable_raw_mode = Term.enable_raw_mode () in
  Fun.protect (fun () -> Input.process_keypress ())
    ~finally:(fun () -> disable_raw_mode ())
