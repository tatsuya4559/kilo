(* row = char gapbuf 
   whole = gapbuf gapbuf
   *)
open Printf

module Array = BatArray
module Char = BatChar
module IO = BatIO
module Option = BatOption

(* いったん char gapbufとして実装してからファンクタにする *)
type t = {
  mutable buf: char option array;
  mutable gap_size: int;
  mutable gap_offset: int;
}

(** debug print for t *)
let debug t =
  let string_of_array = IO.to_string (Array.print (Option.print Char.print)) in
  sprintf "GapBuffer { buf = %s; gap_size = %d; gap_offset = %d; }"
    (string_of_array t.buf) t.gap_size t.gap_offset

let buf_equal = Array.equal (Option.eq ~eq:Char.equal)

let equal t1 t2 =
  t1.gap_size = t2.gap_size
    && t1.gap_offset = t2.gap_offset
    && buf_equal t1.buf t2.buf

let create size =
  {
    buf = Array.make size None;
    gap_size = size;
    gap_offset = 0;
  }

let content_length t =
  Array.length t.buf - t.gap_size

let at t offset =
  if content_length t <= offset then
    None
  else begin
    assert (offset < (Array.length t.buf - t.gap_size));
    if offset < t.gap_offset then
      match Array.get t.buf offset with
      | Some ch -> Some ch
      | None -> assert false
    else
      Array.get t.buf (offset + t.gap_size)
  end

let insert t offset elem =
  t.buf.(offset) <- Some elem;
  t.gap_size <- t.gap_size - 1;
  t.gap_offset <- t.gap_offset + 1

let%test_module "gap_buffer test" = (module struct

  let assert_buf_equal got want =
    if equal got want then
      true
    else
      failwith (sprintf "gap_buffer: want %s, got %s" (debug want) (debug got))


  let%test "create" =
    let gapbuf = create 3 in
    assert_buf_equal gapbuf {
      buf = [|None; None; None|];
      gap_size = 3;
      gap_offset = 0;
    }

  let%test "at None" = 
    let gapbuf = create 3 in
    at gapbuf 1 = None

  let%test "insert at 0" =
    let gapbuf = create 5 in
    insert gapbuf 0 'a';
    assert_buf_equal gapbuf {
      buf = [|Some 'a'; None; None; None; None|];
      gap_size = 4;
      gap_offset = 1;
    }
  let%test "insert at 0, 1" =
    let gapbuf = create 5 in
    insert gapbuf 0 'a';
    insert gapbuf 1 'b';
    assert_buf_equal gapbuf {
      buf = [|Some 'a'; Some 'b'; None; None; None|];
      gap_size = 3;
      gap_offset = 2;
    }

end)
