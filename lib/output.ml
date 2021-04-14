let write = output_string stdout
let flush () = flush stdout

let draw_rows () =
  for _ = 0 to 23 do
    write "~\r\n"
  done

let refresh_screen () =
  write "\x1b[2J";
  write "\x1b[H";
  draw_rows ();
  write "\x1b[H";
  flush ()
