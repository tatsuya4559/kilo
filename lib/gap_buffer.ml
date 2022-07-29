(* row = char gapbuf 
   whole = gapbuf gapbuf
   *)
open Printf
module Array = BatArray
module Char = BatChar
module IO = BatIO
module Option = BatOption

(* module type ElementType = sig
  type t
  val print : 'a BatInnerIO.output -> t -> unit
  val equal : t -> t -> bool
end *)

(*
offset means index in t.buf array = internal use
index means index of content elements = interface use
*)

(* いったん char gapbufとして実装してからファンクタにする *)
type t = {
  mutable buf : char option array;
  mutable gap_size : int;
  mutable gap_offset : int;
}

(* TODO: use ppx_derving.show *)

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

let make size = { buf = Array.make size None; gap_size = size; gap_offset = 0 }

let index_to_offset t index =
  if index <= t.gap_offset then
    index
  else
    index + t.gap_size

let at t index =
  if index < t.gap_offset then
    t.buf.(index)
  else
    t.buf.(index + t.gap_size)

let content_length t = Array.length t.buf - t.gap_size

let glow_buf_size t =
  let size = Array.length t.buf in
  assert (t.gap_size = 0);
  t.buf <- Array.append t.buf (Array.make size None);
  t.gap_offset <- size;
  t.gap_size <- t.gap_size + size

let reserve_buf_size t size =
  if Array.length t.buf >= size then
    ()
  else
    glow_buf_size t

let move_gap t offset =
  if offset >= Array.length t.buf then
    failwith "move_gap offset out of range"
  else if offset = t.gap_offset then
    ()
  else if offset < t.gap_offset then (
    (* when gap is after cursor *)
    (* xx|xoooxxx*)
    t.buf <-
      Array.concat
        [
          Array.sub t.buf 0 offset;
          Array.sub t.buf t.gap_offset t.gap_size;
          Array.sub t.buf offset (t.gap_offset - offset);
          Array.sub t.buf
            (t.gap_offset + t.gap_size)
            (Array.length t.buf - (t.gap_offset + t.gap_size));
        ];
    t.gap_offset <- offset
  ) else
    (* when gap is before cursor *)
    (* xxxooox|xx*)
    t.buf <-
      Array.concat
        [
          Array.sub t.buf 0 t.gap_offset;
          Array.sub t.buf
            (t.gap_offset + t.gap_size)
            (offset - (t.gap_offset + t.gap_size));
          Array.sub t.buf t.gap_offset t.gap_size;
          Array.sub t.buf offset (Array.length t.buf - offset);
        ];
  t.gap_offset <- offset

(* when cursor is mid of gap (is this possible?) *)
(* xxxo|ooxxx*)

let insert t index elem =
  if index < 0 || content_length t < index then
    failwith "out of range"
  else (
    reserve_buf_size t (content_length t + 1);
    (* index_to_offset関数はいらなくて、単にmove_gapの考慮が足りていないみたい*)
    let offset = index_to_offset t index in
    move_gap t offset;
    t.buf.(index) <- Some elem;
    t.gap_size <- t.gap_size - 1;
    t.gap_offset <- t.gap_offset + 1
  )

let%test_module "gap_buffer test" =
  (module struct
    let assert_buf_equal got want =
      if equal got want then
        ()
      else
        failwith
          (sprintf "gap_buffer: want %s, got %s" (debug want) (debug got))

    let%test_unit "create" =
      let gapbuf = make 3 in
      assert_buf_equal gapbuf
        { buf = [| None; None; None |]; gap_size = 3; gap_offset = 0 }

    let%test "get element before gap" =
      let gapbuf = make 3 in
      insert gapbuf 0 'a';
      (* 0    1     2     *)
      (* 'a'; None; None; *)
      at gapbuf 0 = Some 'a'

    let%test "get element after gap" =
      let gapbuf = make 3 in
      insert gapbuf 0 'a';
      move_gap gapbuf 0;
      (* 0     1     2    *)
      (* None; None; 'a'; *)
      at gapbuf 0 = Some 'a'

    let%test "content length" =
      let gapbuf = make 3 in
      insert gapbuf 0 'a';
      content_length gapbuf = 1

    let%test_unit "glow buf size" =
      let gapbuf = make 2 in
      insert gapbuf 0 'a';
      insert gapbuf 1 'b';
      glow_buf_size gapbuf;
      assert_buf_equal gapbuf
        {
          buf = [| Some 'a'; Some 'b'; None; None |];
          gap_size = 2;
          gap_offset = 2;
        }

    let%test_unit "insert at 0, 1" =
      let gapbuf = make 5 in
      insert gapbuf 0 'a';
      assert_buf_equal gapbuf
        {
          buf = [| Some 'a'; None; None; None; None |];
          gap_size = 4;
          gap_offset = 1;
        };
      insert gapbuf 1 'b';
      assert_buf_equal gapbuf
        {
          buf = [| Some 'a'; Some 'b'; None; None; None |];
          gap_size = 3;
          gap_offset = 2;
        }
  end)
