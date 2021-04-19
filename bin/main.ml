open Kilo
open Term

let () =
  let argv = Sys.argv in
  Term.with_raw_mode (fun () ->
    let editor_config =
      match Editor_config.create () with
      | None -> Term.die "fail to create editor config"
      | Some x ->
          if Array.length argv >= 2 then
            let filename = argv.(1) in
            Editor_config.open_file x filename
          else
            x
    in
    Editor_config.set_statusmsg editor_config "HELP: Ctrl-S = save | Ctrl-Q = quit";
    Editor_config.process_keypress editor_config)
