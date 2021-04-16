open Printf

let kilo_version = "0.1"

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
  let erase_right_of_cursor = "\x1b[K"
  (* y and x are indexes that start from 0 *)
  let move_cursor y x = sprintf "\x1b[%d;%dH" (y+1) (x+1)
end

let write = output_string stdout
let flush () = flush stdout

let ctrl c =
  Char.chr ((Char.code c) land 0x1f)

let die msg =
  write Escape_command.clear_screen;
  write Escape_command.cursor_topleft;
  flush ();
  eprintf "%s\n" msg;
  exit 1

type arrow_key = [
  | `Arrow_up
  | `Arrow_down
  | `Arrow_right
  | `Arrow_left
]

type key = [
  | arrow_key
  | `Ch of char
]

let read_key () =
  try
    let c = input_char stdin in
    if c = '\x1b' (* escape *) then begin
      try
        let first = input_char stdin in
        let second = input_char stdin in
        (* convert arrow keys to hjkl *)
        match (first, second) with
        | '[', 'A' -> `Arrow_up
        | '[', 'B' -> `Arrow_down
        | '[', 'C' -> `Arrow_right
        | '[', 'D' -> `Arrow_left
        | _, _ -> `Ch '\x1b'
      with End_of_file (* time out *) -> `Ch '\x1b'
    end else
      `Ch c
  with End_of_file -> `Ch '\000'

module Editor_config = struct
  type t = {
    screenrows: int;
    screencols: int;
    (* cursor position *)
    mutable cx: int;
    mutable cy: int;
  }

  let create () =
    let open Option_monad in
    let* rows = Terminal_size.get_rows () in
    let* cols = Terminal_size.get_columns () in
    Some { screenrows = rows; screencols = cols; cx = 0; cy = 0 }

  let welcome_string width =
      let welcome = sprintf "Kilo editor -- version %s" kilo_version in
      let padding = String.make (((width - String.length welcome) / 2) - 1) ' ' in
      "~" ^ padding ^ welcome

  let draw_rows t =
    for y = 1 to t.screenrows do
      let row =
        if y = t.screenrows / 3 then welcome_string t.screencols
        else "~"
      in
      write row;
      write Escape_command.erase_right_of_cursor;
      if y < t.screenrows then
        write "\r\n"
    done

  let refresh_screen t =
    write Escape_command.hide_cursor;
    write Escape_command.cursor_topleft;
    draw_rows t;

    write @@ Escape_command.move_cursor t.cy t.cx;
    write Escape_command.show_cursor;
    flush ()

  let rec process_keypress t =
    refresh_screen t;
    match read_key () with
    (* quit *)
    | `Ch c when c = ctrl 'q' ->
        write Escape_command.clear_screen;
        write Escape_command.cursor_topleft;
        flush ()
    (* move cursor *)
    | `Arrow_up | `Ch 'k' -> t.cy <- t.cy - 1; process_keypress t
    | `Arrow_down | `Ch 'j' -> t.cy <- t.cy + 1; process_keypress t
    | `Arrow_right | `Ch 'l' -> t.cx <- t.cx + 1; process_keypress t
    | `Arrow_left | `Ch 'h' -> t.cx <- t.cx - 1; process_keypress t
    | _ -> process_keypress t
end
