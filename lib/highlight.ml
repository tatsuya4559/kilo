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

  let default = {
    filetype = "no filetype";
    filematch = [];
    flags = Highlight_flag_set.empty;
  }

  (* syntax database *)
  let db = [
    { filetype = "ocaml";
      filematch = [".ml"; ".mli"];
      flags = Highlight_flag_set.of_list [Highlight_numbers];
    };
  ]

  let detect_filetype filename =
    let extension = Filename.extension filename in
    match List.filter (fun syntax -> List.mem extension syntax.filematch) db with
    | [] -> default
    | hd :: _ -> hd

end

type highlight_group =
  | Normal
  | Number
  | Match

let color_of_hlgroup = function
  | Normal -> Terminal.Default
  | Number -> Terminal.Red
  | Match -> Terminal.Inv_yellow

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
        ^ (Terminal.Escape_command.color @@ color_of_hlgroup hl)
        ^ (String.make 1 ch)
    end
  ) (String.to_seq text) (List.to_seq hlgroups);
  colored_text := !colored_text ^ (Terminal.Escape_command.color Terminal.Default);
  !colored_text

let highlight ~matching syntax text =
  colorize text (get_hlgroups ~matching syntax text)
