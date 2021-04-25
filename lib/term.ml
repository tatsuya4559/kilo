open Printf

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

type color =
  | Default
  | Red
  | Blue
  | Inv_yellow

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
  let color = function
    | Default -> "\x1b[39m\x1b[m"
    | Red -> "\x1b[31m"
    | Blue -> "\x1b[34m"
    | Inv_yellow -> "\x1b[33m\x1b[7m"
end

module Highlight = struct
  type highlight_flag =
    | Highlight_numbers

  module Highlight_flag_set =
    BatSet.Make(struct
      type t = highlight_flag
      let compare = Stdlib.compare
    end)

  module Syntax = struct
    type t = {
      filetype: string;
      filematch: string list;
      flags: Highlight_flag_set.t;
    }

    let create filetype filematch flag_list =
      { filetype; filematch; flags = Highlight_flag_set.of_list flag_list }

    (* syntax database *)
    let db = [
      create "ocaml" ["ml"; "mli"] [Highlight_numbers];
    ]
  end

  type highlight_group =
    | Normal
    | Number
    | Match

  let color_of_hlgroup = function
    | Normal -> Default
    | Number -> Red
    | Match -> Inv_yellow

  let is_delimiter c =
    BatChar.is_whitespace c || String.contains ",.()+-/*=~%<>[];" c

  let is_number c is_prev_delimiter prev_hl =
    (BatChar.is_digit c && (is_prev_delimiter || (prev_hl = Number)))
    || (c = '.' && prev_hl = Number)

  (* get_hlgroups returns a list of highlight group.
   * Each element is corresponding to respective chars in text *)
  let get_hlgroups ~matching (syntax:Syntax.t) text =
    let match_pos, match_len =
      try
        (BatString.find text matching, String.length matching)
      with Not_found -> (0, 0)
    in
    let rec loop i hlgroups =
      let is_prev_delimiter = if i = 0 then true else is_delimiter text.[i-1] in
      let prev_hl = match hlgroups with
        | [] -> Normal
        | hd :: _ -> hd
      in
      if i = String.length text then
        List.rev hlgroups
      else if match_pos <= i && i < match_pos + match_len then
        loop (i+1) (Match :: hlgroups)
      else if Highlight_flag_set.mem Highlight_numbers syntax.flags &&
        is_number text.[i] is_prev_delimiter prev_hl then
        loop (i+1) (Number :: hlgroups)
      else
        loop (i+1) (Normal :: hlgroups)
    in
    loop 0 []

  let colorize text hlgroups =
    assert (String.length text = List.length hlgroups);
    let colored_text = ref "" in
    let prev_hl = ref Normal in
    BatSeq.iter2 (fun ch hl ->
      if hl = !prev_hl then begin
        colored_text := !colored_text ^ (String.make 1 ch)
      end else begin
        prev_hl := hl;
        colored_text := !colored_text
          ^ (Escape_command.color @@ color_of_hlgroup hl)
          ^ (String.make 1 ch)
      end
    ) (String.to_seq text) (List.to_seq hlgroups);
    colored_text := !colored_text ^ (Escape_command.color Default);
    !colored_text

  let highlight ~matching syntax text =
    colorize text (get_hlgroups ~matching syntax text)
end

let write = output_string stdout
let flush () = flush stdout

let die msg =
  write Escape_command.clear_screen;
  write Escape_command.cursor_topleft;
  flush ();
  eprintf "%s\n" msg;
  exit 1

(* TODO: distinguish alt keys *)
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
  | Tab
  | Ctrl of char (* char must be a capital case *)
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
      | '\x7f' -> Backspace
      | '\t' -> Tab
      | c when '\x01' <= c && c <= '\x1a' -> Ctrl (Char.chr (Char.code c + 64))
      | _ -> Ch c
  with End_of_file -> Nothing

type direction = Forward | Backward

type search_context = {
  mutable direction: direction;
  mutable query: string;
  mutable matching: string;
}
