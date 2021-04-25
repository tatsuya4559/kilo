open Printf
open Terminal

type direction = Forward | Backward

type search_context = {
  mutable direction: direction;
  mutable query: string;
  mutable matching: string;
}

type t = {
  screenrows: int;
  screencols: int;

  (* cursor position *)
  mutable cx: int; (* visual position *)
  mutable cy: int;

  (* offsets *)
  mutable rowoff: int;
  mutable coloff: int;

  (* contents *)
  mutable filename: string option;
  buf: Editor_buffer.t;
  mutable dirty: bool;
  mutable syntax: Highlight.Syntax.t;

  (* status message *)
  mutable statusmsg: string;
  mutable statusmsg_time: float; (* UNIX time in seconds *)

  mutable quitting_count: int;

  search_context: search_context;
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
         syntax = Highlight.Syntax.default;
         statusmsg = "";
         statusmsg_time = 0.;
         quitting_count = 0;
         search_context = {
           direction = Forward;
           query = "";
           matching = "";
         };
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
  { t with
    filename = Some filename;
    buf;
    syntax = Highlight.Syntax.detect_filetype filename;
  }

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
        Editor_buffer.get_sub t.buf ~y:filerow ~x:t.coloff ~len:t.screencols
        |> Highlight.highlight ~matching:t.search_context.matching t.syntax
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

let fit ?(pad=' ') len ~left ~right =
  let left_len = String.length left in
  let right_len = String.length right in
  if (left_len + right_len) <= len then
    let padding = String.make (len - left_len - right_len) pad in
    left ^ padding ^ right
  else
    StringLabels.sub left ~pos:0 ~len:(len - right_len) ^ right

let draw_status_bar t =
  let bar = fit t.screencols
    ~left:(sprintf "%s%s - %d lines - %d cols"
      (BatOption.default "[No Name]" t.filename)
      (if t.dirty then " [+]" else "")
      (rows t.buf)
      (cols t.buf t.cy))
    ~right:(sprintf "%s | %d/%d" t.syntax.filetype (t.cy + 1) (rows t.buf))
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
    | `Right ->
        if Editor_buffer.is_tab t.buf ~y:t.cy ~x:(t.cx + 1) then
          t.cx + Settings.kilo_tabstop, t.cy
        else
          t.cx + 1, t.cy
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
  let cx = if cx < 0 then 0 else if cx > cols t.buf t.cy then cols t.buf t.cy else cx in
  t.cx <- Editor_buffer.adjust_x_pos t.buf ~y:t.cy ~x:cx

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
  t.cx <- t.cx + if c = '\t' then Settings.kilo_tabstop else 1;
  t.dirty <- true

let insert_newline t =
  Editor_buffer.insert_newline t.buf ~y:t.cy ~x:t.cx;
  t.cy <- t.cy + 1;
  t.cx <- 0

let delete_char t =
  if t.cy = rows t.buf then ()
  else if t.cy = 0 && t.cx = 0 then ()
  else if t.cx > 0 then begin
    let is_tab = Editor_buffer.is_tab t.buf ~y:t.cy ~x:(t.cx - 1) in
    Editor_buffer.delete_char t.buf ~y:t.cy ~x:(t.cx - 1);
    t.cx <- t.cx - if is_tab then Settings.kilo_tabstop else 1;
    t.dirty <- true
  end else begin
    let new_cx = cols t.buf (t.cy - 1) in
    Editor_buffer.join_row t.buf (t.cy - 1);
    t.cy <- t.cy - 1;
    t.cx <- new_cx;
    t.dirty <- true
  end

let prompt ?(callback=(fun _ -> ())) t msgfmt =
  let rec prompt' t input =
    callback input;
    set_statusmsg t (sprintf msgfmt input);
    refresh_screen t;
    match read_key () with
    | Enter -> set_statusmsg t ""; Some input
    | Esc -> set_statusmsg t ""; None
    | Backspace ->
        prompt' t (BatString.rchop ~n:1 input)
    | Ctrl 'H' ->
        prompt' t (BatString.rchop ~n:1 input)
    | Ch c ->
        prompt' t (sprintf "%s%c" input c)
    | _ -> prompt' t input
  in
  prompt' t ""

let search_and_jump t start_y query =
  let rec search_row y =
    match Editor_buffer.get_position_in_row t.buf y query with
    | -1 ->
        (match t.search_context.direction with
        | Forward -> if y < rows t.buf - 1 then search_row (y+1)
        | Backward -> if y > 0 then search_row (y-1))
    | x ->
        t.search_context.matching <- query;
        t.cy <- y;
        t.cx <- x
  in
  if query <> "" then search_row start_y

let find t =
  t.search_context.direction <- Forward;
  let orig_x, orig_y = t.cx, t.cy in
  match prompt t "Search: %s (ESC to cancel)" ~callback:(search_and_jump t t.cy) with
  | None -> (* reset position *)
      t.search_context.matching <- "";
      t.cx <- orig_x;
      t.cy <- orig_y
  | Some query -> (* save query *)
      t.search_context.query <- query

let find_next t =
  t.search_context.direction <- Forward;
  let start_y = if t.cy >= rows t.buf - 1 then rows t.buf - 1 else t.cy + 1 in
  search_and_jump t start_y t.search_context.query

let find_prev t =
  t.search_context.direction <- Backward;
  let start_y = if t.cy <= 0 then 0 else t.cy - 1 in
  search_and_jump t start_y t.search_context.query

let get_filename t =
  if Option.is_some t.filename then
    t.filename
  else
    match prompt t "Save as: %s (Esc to cancel)" with
    | Some _ as f -> t.filename <- f; f
    | None -> None

let save_file t =
  let open BatFile in
  match get_filename t with
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
    | Ctrl 'Q' ->
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
    (* save file *)
    | Ctrl 'S' -> save_file t; `Continue
    (* search *)
    | Ctrl 'F' -> find t; `Continue
    | Ctrl 'N' -> find_next t; `Continue
    | Ctrl 'P' -> find_prev t; `Continue
    (* insert/delete text *)
    | Enter -> insert_newline t; `Continue
    | Tab -> insert_char t '\t'; `Continue
    | Del -> move_cursor t `Right; delete_char t; `Continue
    | Backspace -> delete_char t; `Continue
    | Ctrl 'H' -> delete_char t; `Continue
    | Ctrl 'D' -> delete_row t; `Continue
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
