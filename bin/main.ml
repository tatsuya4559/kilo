open Kilo

let () =
  let argv = Sys.argv in
  Terminal.with_raw_mode (fun () ->
    let editor =
      match Editor.create () with
      | None -> Terminal.die "fail to create editor config"
      | Some editor ->
          if Array.length argv >= 2 then
            let filename = argv.(1) in
            Editor.open_file editor filename
          else
            editor
    in
    Editor.set_statusmsg editor "HELP: ^S = save | ^Q = quit | ^F = find";
    Editor.process_keypress editor)
