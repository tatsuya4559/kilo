open Printf
open Util

(* constants *)
let kilo_version = "0.1"
let kilo_tabstop = 8


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

(* TODO: gap buffer *)
module Editor_buffer : sig
  type t
  val create : unit -> t
  val numrows : t -> int
  val numcols : t -> int -> int
  val append_row : t -> string -> t
  val get : y:int -> x:int -> len:int -> t -> string
  val insert_char : t -> char -> y:int -> x:int -> t
end = struct
  let rendered_tab = String.make kilo_tabstop ' '

  (*** row ***)
  type row = {
    (* actual string *)
    raw_string: string;
    (* string to be rendered *)
    render_string: string;
  }

  let render raw =
    (* render a tab as `kilo_tabstop` spaces *)
    BatString.nreplace ~str:raw ~sub:"\t" ~by:rendered_tab

  let raw_length row =
    String.length row.raw_string

  let slice s start stop =
    StringLabels.sub s ~pos:start ~len:(stop - start)

  let insert_char_to_row row at c =
    let at = if at < 0 || at > raw_length row then raw_length row else at in
    let raw_string = (slice row.raw_string 0 at)
      ^ Char.escaped c
      ^ (slice row.raw_string at (raw_length row))
    in
    { raw_string; render_string = render raw_string }

  (*** buffer ***)
  (* buffer state should be immutable for undoing *)
  type t = {
    content: row list;
  }

  let create () = { content = [] }

  let numrows t = List.length t.content
  let numcols t y =
    match List.nth_opt t.content y with
    | None -> 0
    | Some row -> String.length row.render_string

  let append_row t raw_string =
    { content = t.content @ [{raw_string; render_string = render raw_string }] }

  (** get contents of buffer that starts at (`x`, `y`) and has `len` length at most.
   *  x and y are indexes from 0. *)
  let get ~y ~x ~len t =
    let row_str = (List.nth t.content y).render_string in
    let row_len = String.length row_str in
    if row_len <= x then
      ""
    else
      StringLabels.sub row_str ~pos:x ~len:(BatInt.min len (row_len - x))

  let insert_char t c ~y ~x =
    let before, row, after =
      match BatList.takedrop y t.content with
      | before, row :: after -> before, row, after
      | _, _ -> assert false
    in
    let row = insert_char_to_row row x c in
    { content = before @ [row] @ after }
end

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
    filename: string;
    mutable buf: Editor_buffer.t;
    (* status message *)
    mutable statusmsg: string;
    mutable statusmsg_time: float; (* UNIX time in seconds *)
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
           filename = "[No Name]";
           buf = Editor_buffer.create ();
           statusmsg = "";
           statusmsg_time = 0.;
         }

  (* shorthand for Editor_buffer.numrows *)
  let numrows t =
    Editor_buffer.numrows t.buf

  (* shorthand for Editor_buffer.numcols *)
  let numcols t =
    Editor_buffer.numcols t.buf t.cy

  let set_statusmsg t msg =
    t.statusmsg <- msg;
    t.statusmsg_time <- Unix.time ()

  let open_file t filename =
    let buf = BatFile.with_file_in filename (fun input ->
      let rec readline input buf =
        try
          readline input (Editor_buffer.append_row buf (BatIO.read_line input))
        with BatIO.No_more_input -> buf
      in
      readline input (Editor_buffer.create ())
    ) in
    { t with filename; buf }

  let welcome_string width =
    let welcome = sprintf "Kilo editor -- version %s" kilo_version in
    let padding = String.make (((width - String.length welcome) / 2) - 1) ' ' in
    "~" ^ padding ^ welcome

  let draw_rows t =
    for y = 0 to t.screenrows - 1 do
      let filerow = y + t.rowoff in
      let row =
        (* text buffer *)
        if filerow < numrows t then
          Editor_buffer.get t.buf ~y:filerow ~x:t.coloff ~len:(t.screencols)
        (* welcome text *)
        else if numrows t = 0 && y = t.screenrows / 3 then welcome_string t.screencols
        (* out of buffer *)
        else "~"
      in
      write row;
      write Escape_command.erase_right_of_cursor;
      write "\r\n"
    done

  let draw_status_bar t =
    let bar = StringFormat.fit t.screencols
      ~left:(sprintf "%s - %d lines" t.filename (numrows t))
      ~right:(sprintf "%d/%d" (t.cy + 1) (numrows t))
    in
    write @@ Escape_command.inverted_text bar;
    write "\r\n"

  let draw_message_bar t =
    write Escape_command.erase_right_of_cursor;
    (* show 5 seconds *)
    write @@ if (Unix.time () -. t.statusmsg_time) < 5.
        then t.statusmsg
        else ""

  (* update rowoff if cursor is out of screen *)
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
      | `Bottom -> t.cx, numrows t
      | `Head -> 0, t.cy
      | `Tail -> numcols t, t.cy
      | `Full_up -> t.cx, t.rowoff - t.screenrows
      | `Full_down -> t.cx, t.rowoff + 2 * t.screenrows - 1
    in
    (* update y first because the max length of row depends on t.cy *)
    t.cy <- if cy < 0 then 0 else if cy > numrows t then numrows t else cy;
    t.cx <- if cx < 0 then 0 else if cx > numcols t then numcols t else cx

  let insert_char t c =
    let () = if t.cy = numrows t then
      t.buf <- Editor_buffer.append_row t.buf ""
    in
    t.buf <- Editor_buffer.insert_char t.buf c ~y:t.cy ~x:t.cx;
    t.cx <- t.cx + 1

  let rec process_keypress t =
    refresh_screen t;
    match read_key () with
    (* quit *)
    | Ch c when c = ctrl 'q' ->
        write Escape_command.clear_screen;
        write Escape_command.cursor_topleft;
        flush ()
    (* move cursor *)
    | Arrow_up -> move_cursor t `Up; process_keypress t
    | Arrow_down -> move_cursor t `Down; process_keypress t
    | Arrow_right -> move_cursor t `Right; process_keypress t
    | Arrow_left -> move_cursor t `Left; process_keypress t
    | Page_up -> move_cursor t `Full_up; process_keypress t
    | Page_down -> move_cursor t `Full_down; process_keypress t
    | Home -> move_cursor t `Head; process_keypress t
    | End -> move_cursor t `Tail; process_keypress t
    | Ch c -> insert_char t c; process_keypress t
    | _ -> process_keypress t
end
