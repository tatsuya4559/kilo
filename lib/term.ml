open Printf
open Util

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
  let bold_text s = sprintf "\x1b[1m%s\x1b[m" s
  let underlined_text s = sprintf "\x1b[4m%s\x1b[m" s
  let blinking_text s = sprintf "\x1b[5m%s\x1b[m" s
  let inverted_text s = sprintf "\x1b[7m%s\x1b[m" s
end

let write = output_string stdout
let flush () = flush stdout

let ctrl c =
  Char.chr ((Char.code c) land 0x1f)

let is_ctrl c =
  let code = Char.code c in
  (0x00 <= code && code <= 0x1f) || code = 0x7f

let die msg =
  write Escape_command.clear_screen;
  write Escape_command.cursor_topleft;
  flush ();
  eprintf "%s\n" msg;
  exit 1

(* TODO: distinguish ctrl, alt keys *)
type key =
  | Nothing
  | Arrow_up
  | Arrow_down
  | Arrow_right
  | Arrow_left
  | Page_up
  | Page_down
  | Home
  | End
  | Del
  | Backspace
  | Enter
  | Esc
  | Ch of char

let read_key () =
  try
    let c = input_char stdin in
    if c = '\x1b' (* escape *) then begin
      try
        let first = input_char stdin in
        let second = input_char stdin in
        match (first, second) with
        | '[', 'A' -> Arrow_up
        | '[', 'B' -> Arrow_down
        | '[', 'C' -> Arrow_right
        | '[', 'D' -> Arrow_left
        | '[', 'H' -> Home
        | '[', 'F' -> End
        | '[', second when '0' <= second && second <= '9' ->
            let third = input_char stdin in
            (match (second, third) with
            | '1', '~' -> Home
            | '3', '~' -> Del
            | '4', '~' -> End
            | '5', '~' -> Page_up
            | '6', '~' -> Page_down
            | '7', '~' -> Home
            | '8', '~' -> End
            | _, _ -> Esc)
        | 'O', 'H' -> Home
        | 'O', 'F' -> End
        | _, _ -> Esc
      with End_of_file (* time out *) -> Esc
    end else
      match c with
      | '\r' -> Enter
      | '\127' -> Backspace
      | _ -> Ch c
  with End_of_file -> Nothing

module Editor_config : sig
  (** global state of editor *)
  type t

  (** create new editor state *)
  val create : unit -> t option

  (** read content from file *)
  val open_file : t -> string -> t

  (** set status message *)
  val set_statusmsg : t -> string -> unit

  (** wait and process keypress *)
  val process_keypress : t -> unit
end = struct
  type t = {
    screenrows: int;
    screencols: int;

    (* cursor position *)
    mutable cx: int;
    mutable cy: int;

    (* offsets *)
    mutable rowoff: int;
    mutable coloff: int;

    (* contents *)
    mutable filename: string option;
    buf: Editor_buffer.t;
    mutable dirty: bool;

    (* status message *)
    mutable statusmsg: string;
    mutable statusmsg_time: float; (* UNIX time in seconds *)

    mutable quitting_count: int;
  }

  let create () =
    let open Option_monad in
    let* rows = Terminal_size.get_rows () in
    let* cols = Terminal_size.get_columns () in
    Some { screenrows = rows - 2; (* -2 to make room for status bar and message *)
           screencols = cols;
           cx = 0;
           cy = 0;
           rowoff = 0;
           coloff = 0;
           filename = None;
           buf = Editor_buffer.create "";
           dirty = false;
           statusmsg = "";
           statusmsg_time = 0.;
           quitting_count = 0;
         }

  (* shorthands *)
  let rows = Editor_buffer.rows
  let cols = Editor_buffer.cols

  let set_statusmsg t msg =
    t.statusmsg <- msg;
    t.statusmsg_time <- Unix.time ()

  let open_file t filename =
    let buf = BatFile.with_file_in filename (fun input ->
      let rec readline input y buf =
        try
          Editor_buffer.append_row buf y (BatIO.read_line input);
          readline input (y+1) buf
        with BatIO.No_more_input -> buf
      in
      try
        readline input 0 (Editor_buffer.create (BatIO.read_line input))
      with BatIO.No_more_input -> Editor_buffer.create ""
    ) in
    { t with filename = Some filename; buf}

  let welcome_string width =
    let welcome = sprintf "Kilo editor -- version %s" Settings.kilo_version in
    let padding = String.make (((width - String.length welcome) / 2) - 1) ' ' in
    "~" ^ padding ^ welcome

  let draw_rows t =
    for y = 0 to t.screenrows - 1 do
      let filerow = y + t.rowoff in
      let row =
        (* text buffer *)
        if filerow < rows t.buf then begin
          Editor_buffer.get t.buf ~y:filerow ~x:t.coloff ~len:t.screencols
        (* welcome text *)
        end else if rows t.buf = 1
          && cols t.buf 0 = 0
          && y = t.screenrows / 3 then welcome_string t.screencols
        (* out of buffer *)
        else "~"
      in
      write row;
      write Escape_command.erase_right_of_cursor;
      write "\r\n"
    done

  let draw_status_bar t =
    let bar = StringFormat.fit t.screencols
      ~left:(sprintf "%s%s - %d lines - %d cols"
        (BatOption.default "[No Name]" t.filename)
        (if t.dirty then " [+]" else "")
        (rows t.buf)
        (cols t.buf t.cy))
      ~right:(sprintf "%d/%d" (t.cy + 1) (rows t.buf))
    in
    write @@ Escape_command.inverted_text bar;
    write "\r\n"

  let draw_message_bar t =
    write Escape_command.erase_right_of_cursor;
    (* show 5 seconds *)
    write @@ if (Unix.time () -. t.statusmsg_time) < 5.
        then t.statusmsg
        else ""

  (* update rowoff and coloff if cursor is out of screen *)
  let scroll t =
    t.rowoff <-
      if t.cy < t.rowoff then t.cy
      else if t.cy >= t.rowoff + t.screenrows then t.cy - t.screenrows + 1
      else t.rowoff;
    t.coloff <-
      if t.cx < t.coloff then t.cx
      else if t.cx >= t.coloff + t.screencols then t.cx - t.screencols + 1
      else t.coloff

  let refresh_screen t =
    scroll t;
    write Escape_command.hide_cursor;
    write Escape_command.cursor_topleft;
    draw_rows t;
    draw_status_bar t;
    draw_message_bar t;
    write @@ Escape_command.move_cursor (t.cy - t.rowoff) (t.cx - t.coloff);
    write Escape_command.show_cursor;
    flush ()

  let move_cursor t dir =
    let cx, cy = match dir with
      | `Up -> t.cx, t.cy - 1
      | `Down -> t.cx, t.cy + 1
      | `Right -> t.cx + 1, t.cy
      | `Left -> t.cx - 1, t.cy
      | `Top -> t.cx, 0
      | `Bottom -> t.cx, rows t.buf
      | `Head -> 0, t.cy
      | `Tail -> cols t.buf t.cy, t.cy
      | `Full_up -> t.cx, t.rowoff - t.screenrows
      | `Full_down -> t.cx, t.rowoff + 2 * t.screenrows - 1
    in
    (* update y first because the max length of row depends on t.cy *)
    t.cy <- if cy < 0 then 0 else if cy > rows t.buf then rows t.buf else cy ;
    t.cx <- if cx < 0 then 0 else if cx > cols t.buf t.cy then cols t.buf t.cy else cx

  let delete_row t =
    if rows t.buf <> 1 then begin
      Editor_buffer.delete_row t.buf t.cy;
      (* if deleted last line then decrement cy *)
      if t.cy = rows t.buf - 1 then t.cy <- t.cy - 1;
      t.dirty <- true
    end

  let insert_char t c =
    if t.cy = rows t.buf then begin
      (* making cyth row is appending a row after (cy - 1) *)
      Editor_buffer.append_row t.buf (t.cy - 1) ""
    end;
    Editor_buffer.insert_char t.buf c ~y:t.cy ~x:t.cx;
    t.cx <- t.cx + 1;
    t.dirty <- true

  let insert_newline t =
    Editor_buffer.insert_newline t.buf ~y:t.cy ~x:t.cx;
    t.cy <- t.cy + 1;
    t.cx <- 0

  let delete_char t =
    if t.cy = rows t.buf then ()
    else if t.cy = 0 && t.cx = 0 then ()
    else if t.cx > 0 then begin
      Editor_buffer.delete_char t.buf ~y:t.cy ~x:(t.cx - 1);
      t.cx <- t.cx - 1;
      t.dirty <- true
    end else begin
      let new_cx = cols t.buf (t.cy - 1) in
      Editor_buffer.join_row t.buf (t.cy - 1);
      t.cy <- t.cy - 1;
      t.cx <- new_cx;
      t.dirty <- true
    end

  let prompt t msgfmt =
    let rec prompt' t input =
      set_statusmsg t (sprintf msgfmt input);
      refresh_screen t;
      match read_key () with
      | Enter -> set_statusmsg t ""; Some input
      | Esc -> set_statusmsg t ""; None
      | Backspace ->
          prompt' t (BatString.rchop ~n:1 input)
      | Ch c when c = ctrl 'h' ->
          prompt' t (BatString.rchop ~n:1 input)
      | Ch c when (not @@ is_ctrl c) && Char.code c < 128 ->
          prompt' t (sprintf "%s%c" input c)
      | _ -> prompt' t input
    in
    prompt' t ""

  let save_file t =
    let open BatFile in
    if Option.is_none t.filename then begin
      match prompt t "Save as: %s (Esc to cancel)" with
      | None -> ()
      | Some filename -> t.filename <- Some filename
    end;
    match t.filename with
    | None ->
        set_statusmsg t "Save aborted"
    | Some filename ->
      let p = perm [user_read; user_write; group_read; other_read] in
      with_file_out ~mode:[`create; `trunc] ~perm:p filename (fun output ->
        Editor_buffer.to_string t.buf
        |> String.iter (fun c -> BatIO.write output c);
        set_statusmsg t @@ sprintf "%s written" filename;
        t.dirty <- false
      )

  let rec process_keypress t =
    refresh_screen t;
    let result = match read_key () with
      (* quit *)
      | Ch c when c = ctrl 'q' ->
          if t.dirty && t.quitting_count < 1 then begin
            t.quitting_count <- t.quitting_count + 1;
            set_statusmsg t "WARNING!!! File has unsaved changes. Press Ctrl-Q again to quit.";
            `Wait
          end else begin
            write Escape_command.clear_screen;
            write Escape_command.cursor_topleft;
            flush ();
            `Quit
          end
      (* move cursor *)
      | Arrow_up -> move_cursor t `Up; `Continue
      | Arrow_down -> move_cursor t `Down; `Continue
      | Arrow_right -> move_cursor t `Right; `Continue
      | Arrow_left -> move_cursor t `Left; `Continue
      | Page_up -> move_cursor t `Full_up; `Continue
      | Page_down -> move_cursor t `Full_down; `Continue
      | Home -> move_cursor t `Head; `Continue
      | End -> move_cursor t `Tail; `Continue
      | Ch c when c = ctrl 'p' -> move_cursor t `Up; `Continue
      | Ch c when c = ctrl 'n' -> move_cursor t `Down; `Continue
      | Ch c when c = ctrl 'f' -> move_cursor t `Right; `Continue
      | Ch c when c = ctrl 'b' -> move_cursor t `Left; `Continue
      | Ch c when c = ctrl 'a' -> move_cursor t `Head; `Continue
      | Ch c when c = ctrl 'e' -> move_cursor t `Tail; `Continue
      (* save file *)
      | Ch c when c = ctrl 's' -> save_file t; `Continue
      (* insert/delete text *)
      | Enter -> insert_newline t; `Continue
      | Del -> move_cursor t `Right; delete_char t; `Continue
      | Backspace -> delete_char t; `Continue
      | Ch c when c = ctrl 'h' -> delete_char t; `Continue
      | Ch c when c = ctrl 'd' -> delete_row t; `Continue
      | Ch c -> insert_char t c; `Continue
      (* no keypress *)
      | _ -> `Wait
    in
    match result with
    | `Quit -> ()
    | `Wait ->
        process_keypress t
    | `Continue ->
        t.quitting_count <- 0;
        process_keypress t
end
