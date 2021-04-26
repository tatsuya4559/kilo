type highlight_group =
  | Normal
  | String
  | Number
  | Match

let color_of_hlgroup = function
  | Normal -> Terminal.Default
  | String -> Terminal.Magenta
  | Number -> Terminal.Red
  | Match -> Terminal.Inv_yellow


type pattern = {
  group: highlight_group;
  regex: string;
}

module Syntax = struct
  type t = {
    filetype: string;
    filematch: string list;
    rule: pattern list;
  }


  let default = {
    filetype = "no filetype";
    filematch = [];
    rule = [];
  }

  (* syntax database *)
  let db = [
    { filetype = "ocaml";
      filematch = [".ml"; ".mli"];
      rule = [
        { group = Number;
          regex = {|\d+\.?\d*|};
        };
        { group = String;
          regex = {|"[^"]*"|};
        };
      ];
    };
  ]

  let detect_filetype filename =
    let extension = Filename.extension filename in
    match List.filter (fun syntax -> List.mem extension syntax.filematch) db with
    | [] -> default
    | hd :: _ -> hd

end


(* textの先頭でpatternsのいずれかにマッチすればgroupとlengthを返す
 * 見つからなければNormal, 1を返す *)
let find_at_beggining patterns text =
  let rec loop patterns =
    match patterns with
    | [] -> Normal, 1
    | pattern :: tl ->
      try
        let found = Pcre.pcre_exec ~pos:0 ~pat:pattern.regex ~flags:[`NOTEMPTY] text in
        let pos, len = found.(0), found.(1) in
        if pos <> 0 then
          loop tl
        else
          pattern.group, len
      with Not_found -> loop tl
  in
  loop patterns

let ocaml_rule = Syntax.detect_filetype ".ml"
let%test _ = find_at_beggining ocaml_rule.rule "12" = (Number, 2)
let%test _ = find_at_beggining ocaml_rule.rule "12." = (Number, 3)
let%test _ = find_at_beggining ocaml_rule.rule "12.32" = (Number, 5)
let%test _ = find_at_beggining ocaml_rule.rule "int32" = (Normal, 1)
let%test _ = find_at_beggining ocaml_rule.rule "\"foo\" 12.32" = (String, 5)
let%test _ = find_at_beggining ocaml_rule.rule "foo" = (Normal, 1)

(* returns a list of highlight group.
 * Each element is corresponding to respective chars in text *)
let get_highlights ~matching patterns text =
  let max = String.length text in
  let rec loop i hlgroups =
    if i >= max then
      List.rev hlgroups
    else
      let group, len = find_at_beggining patterns (BatString.slice ~first:i text) in
      loop (i+len) ((BatList.make len group) @ hlgroups)
  in
  let highlights = loop 0 [] in
  try
    (* overwrite matching highlights *)
    let match_pos, match_len = (BatString.find text matching, String.length matching) in
    (BatList.take match_pos highlights)
      @ (BatList.make match_len Match)
      @ (BatList.drop (match_pos + match_len) highlights)
  with
    Not_found -> highlights

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

let highlight ~matching (syntax:Syntax.t) text =
  colorize text (get_highlights ~matching syntax.rule text)
