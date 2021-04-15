open Kilo
open Term

let () =
  Term.with_raw_mode (fun () ->
    let editor_config =
      match Editor_config.create () with
      | None -> Term.die "fail to create editor config"
      | Some x -> x
    in
    Editor_config.process_keypress editor_config)
