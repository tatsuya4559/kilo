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
let render row =
  let s = DL.get row in
  S.nreplace ~str:s ~sub:"\t" ~by:rendered_tab

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
    S.length @@ render t.curr_row
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
  let before = S.slice ~last:x curr in
  let at_and_after = S.slice ~first:x curr in
  DL.set t.curr_row before;
  append_row t y at_and_after

(* FIXME: render後のxを与えられるが、raw stringのxでinsertしている *)
let insert_char t c ~y ~x =
  move t y;
  let row = DL.get t.curr_row in
  DL.set t.curr_row @@
    (S.slice ~last:x row) ^ (S.make 1 c) ^ (S.slice ~first:x row)

(* FIXME: render後のxを与えられるが、raw stringのxでdeleteしている *)
let delete_char t ~y ~x =
  move t y;
  let row = DL.get t.curr_row in
  DL.set t.curr_row @@ (S.slice ~last:x row) ^ (S.slice ~first:(x+1) row)

let get t ~y ~x ~len =
  move t y;
  let rendered = render t.curr_row in
  let rendered_len = S.length rendered in
  if rendered_len <= x then
    ""
  else
    StringLabels.sub rendered ~pos:x ~len:(BatInt.min len (rendered_len - x))

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
