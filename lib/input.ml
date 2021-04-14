let ctrl c =
  Char.chr ((Char.code c) land 0x1f)

let rec process_keypress () =
  match Term.get_char () with
  | c when c = ctrl 'q' ->
      Output.write "\x1b[2J";
      Output.write "\x1b[H";
      Output.flush ()
  | _ -> process_keypress ()
