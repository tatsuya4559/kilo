let ctrl c =
  Char.chr ((Char.code c) land 0x1f)

let rec process_keypress () =
  match Term.get_char () with
  | c when c = ctrl 'q' -> Output.refresh_screen ()
  | _ -> process_keypress ()
