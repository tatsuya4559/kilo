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

let () =
  let rec loop () =
    match get_char () with
    | 'q' -> ()
    | c -> Printf.printf "%d (%c)\r\n%!" (Char.code c) c; loop ()
  in
  let disable_raw_mode = enable_raw_mode () in
  Fun.protect (fun () -> loop ())
    ~finally:(fun () -> disable_raw_mode ())
