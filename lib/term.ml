(* returns reset function *)
let enable_raw_mode () =
  let open Unix in
  let termios = tcgetattr stdin in
  tcsetattr stdin TCSAFLUSH
  (* how to turn off IEXTEN ? *)
    { termios with
      c_brkint = false;
      c_inpck = false;
      c_istrip = false;
      c_ixon = false;
      c_icrnl = false;
      c_opost = false;
      c_echo = false;
      c_icanon = false;
      c_isig = false;
      c_csize = 8;
      c_vmin = 0;
      c_vtime = 1;
    };
  (fun () -> tcsetattr stdin TCSAFLUSH termios)

let get_char () =
  try input_char stdin
  with End_of_file -> '\000'

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

let ctrl c =
  Char.chr ((Char.code c) land 0x1f)

let rec process_keypress () =
  match get_char () with
  | c when c = ctrl 'q' ->
      write "\x1b[2J";
      write "\x1b[H";
      flush ()
  | _ -> process_keypress ()
