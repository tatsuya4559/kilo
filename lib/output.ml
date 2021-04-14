let refresh_screen () =
  output_string stdout "\x1b[2J";
  flush stdout
