let with_raw_mode fn =
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
  Fun.protect fn ~finally:(fun () -> tcsetattr stdin TCSAFLUSH termios)

module Escape_command = struct
  let clear_screen = "\x1b[2J"
  let cursor_topleft = "\x1b[H"
  let hide_cursor = "\x1b[?25l"
  let show_cursor = "\x1b[?25h"
end

let write = output_string stdout
let flush () = flush stdout

let die msg =
  write Escape_command.clear_screen;
  write Escape_command.cursor_topleft;
  flush ();
  Printf.eprintf "%s\n" msg;
  exit 1

let get_char () =
  try input_char stdin
  with End_of_file -> '\000'

module Editor_config = struct
  type t = {
    screenrows: int;
    screencols: int;
  }

  let create () =
    let open Option_monad in
    let* rows = Terminal_size.get_rows () in
    let* cols = Terminal_size.get_columns () in
    Some { screenrows = rows; screencols = cols }

  let draw_rows t =
    for _ = 1 to t.screenrows - 1 do
      write "~\r\n"
    done;
    write "~"

  let refresh_screen t =
    write Escape_command.hide_cursor;
    write Escape_command.clear_screen;
    write Escape_command.cursor_topleft;
    draw_rows t;
    write Escape_command.cursor_topleft;
    write Escape_command.show_cursor;
    flush ()
end


let ctrl c =
  Char.chr ((Char.code c) land 0x1f)

let rec process_keypress () =
  match get_char () with
  | c when c = ctrl 'q' ->
      write Escape_command.clear_screen;
      write Escape_command.cursor_topleft;
      flush ()
  | _ -> process_keypress ()
