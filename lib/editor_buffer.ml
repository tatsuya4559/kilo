module DL = BatDllist
module S = BatString

(* TODO: 行ごとの内容をgap bufferで保持する *)
type t = {
  first_row: string DL.t;
  mutable curr_row: string DL.t;
  mutable curr_rownum: int;
}

let create s =
  let row = DL.create s in
  { first_row = row;
    curr_row = row;
    curr_rownum = 0;
  }

let rendered_tab = S.make Settings.kilo_tabstop ' '
let render raw_text =
  S.nreplace ~str:raw_text ~sub:"\t" ~by:rendered_tab

let to_visual_x raw_text x =
  let vx = ref 0 in
  for i = 0 to x - 1 do
    if i < S.length raw_text && S.get raw_text i = '\t' then
      vx := !vx + Settings.kilo_tabstop
    else
      vx := !vx + 1
  done;
  !vx

let to_real_x raw_text vx =
  let x = ref 0 in
  let i = ref 0 in
  while !i < vx do
    if !x < S.length raw_text && S.get raw_text !x = '\t' then
      i := !i + Settings.kilo_tabstop
    else
      i := !i + 1;
    if !i <= vx then
      x := !x + 1
  done;
  !x

let%test_module "Renderer test" = (module struct
  let%test "render" =
    render "hoge\tfuga" = "hoge        fuga"

  let%test _ =
    to_visual_x "hoge\tfuga" 9 = 16

  let%test _ =
    to_visual_x "hoge\tfuga" 3 = 3

  let%test _ =
    to_visual_x "hoge\tfuga" 6 = 13

  let%test _ =
    to_real_x "hoge\tfuga" 3 = 3

  let%test _ =
    to_real_x "hoge\tfuga" 13 = 6

  let%test _ =
    to_real_x "hoge\tfuga" 11 = 4

  let%test _ =
    to_real_x "hoge\tfuga" 16 = 9
end)
let rows t = DL.length t.first_row

(** move current line to y *)
let move t y =
  if 0 <= y && y < rows t then begin
    let dy = y - t.curr_rownum in
    t.curr_row <- DL.skip t.curr_row dy;
    t.curr_rownum <- y
  end else assert false (* for debug *)

let cols t y =
  if y >= rows t then
    0
  else begin
    move t y;
    S.length (DL.get t.curr_row |> render)
  end

let append_row t y row =
  move t y;
  DL.add t.curr_row row

let delete_row t y =
  move t y;
  if y = rows t - 1 then begin
    DL.remove t.curr_row;
    t.curr_row <- DL.prev t.first_row;
    t.curr_rownum <- y - 1
  end else
    t.curr_row <- DL.drop t.curr_row

let append_string t y str =
  move t y;
  let curr = DL.get t.curr_row in
  DL.set t.curr_row @@ curr ^ str

let join_row t y =
  if y < rows t - 1 then begin
    move t y;
    append_string t y (DL.get (DL.next t.curr_row));
    delete_row t (y + 1)
  end

let insert_newline t ~y ~x =
  move t y;
  let curr = DL.get t.curr_row in
  let x = to_real_x curr x in
  let before = S.slice ~last:x curr in
  let at_and_after = S.slice ~first:x curr in
  DL.set t.curr_row before;
  append_row t y at_and_after

let insert_char t c ~y ~x =
  move t y;
  let row = DL.get t.curr_row in
  let x = to_real_x row x in
  DL.set t.curr_row @@
    (S.slice ~last:x row) ^ (S.make 1 c) ^ (S.slice ~first:x row)

let delete_char t ~y ~x =
  move t y;
  let row = DL.get t.curr_row in
  let x = to_real_x row x in
  DL.set t.curr_row @@ (S.slice ~last:x row) ^ (S.slice ~first:(x+1) row)

let get_row t y =
  move t y;
  DL.get t.curr_row |> render

let get t ~y ~x ~len =
  move t y;
  let row = DL.get t.curr_row |> render in
  let row_len = S.length row in
  if row_len <= x then
    ""
  else
    StringLabels.sub row ~pos:x ~len:(BatInt.min len (row_len - x))

let get_position_in_row t y query =
  move t y;
  let row = DL.get t.curr_row in
  try
    BatString.find row query
  with Not_found -> -1

let is_tab t ~y ~x =
  move t y;
  let curr = DL.get t.curr_row in
  let x = to_real_x curr x in
  S.length curr > x && curr.[x] = '\t'

let fix_pos t ~y ~x =
  if y >= rows t then
    0
  else begin
    move t y;
    let curr = DL.get t.curr_row in
    to_real_x curr x |> to_visual_x curr
  end

let to_string t =
  let open Settings in
  S.concat kilo_linesep (DL.to_list t.first_row) ^ kilo_linesep

let%test_module "tests" = (module struct
  let%test_unit _ =
    let b = create "hoge" in
    append_row b 0 "fuga";
    append_row b 1 "piyo";
    assert (to_string b = {|hoge
fuga
piyo
|});

    join_row b 1;
    assert (to_string b = {|hoge
fugapiyo
|});

    insert_newline b ~y:1 ~x:2;
    assert (to_string b = {|hoge
fu
gapiyo
|});

    insert_newline b ~y:0 ~x:0;
    assert (to_string b = {|
hoge
fu
gapiyo
|})

end)
