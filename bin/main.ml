open Kilo

let () =
  let argv = Sys.argv in
  Term.with_raw_mode (fun () ->
    let editor_config =
      match Editor.create () with
      | None -> Term.die "fail to create editor config"
      | Some x ->
          if Array.length argv >= 2 then
            let filename = argv.(1) in
            Editor.open_file x filename
          else
            x
    in
    Editor.set_statusmsg editor_config "HELP: ^S = save | ^Q = quit | ^F = find";
    Editor.process_keypress editor_config)
